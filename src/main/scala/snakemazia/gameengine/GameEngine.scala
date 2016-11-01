package snakemazia.gameengine

import snakemazia.gamelogic._
import com.jme3.app.SimpleApplication
import com.jme3.scene.{Spatial, Geometry, Node}
import java.awt.Point
import com.jme3.scene.shape.{Sphere, Box}
import com.jme3.math._
import com.jme3.util.{SkyFactory, TangentBinormalGenerator}
import com.jme3.light.DirectionalLight
import snakemazia.gamelogic.entities._
import collection.mutable.{Map => MMap}
import com.jme3.input.controls.{ActionListener, KeyTrigger, MouseButtonTrigger}
import com.jme3.input.{ChaseCamera, MouseInput, KeyInput}
import com.jme3.font.{BitmapFont, BitmapText}
import com.jme3.renderer.queue.RenderQueue
import com.jme3.renderer.Camera
import java.io.FileInputStream
import java.util.Properties
import com.jme3.post.FilterPostProcessor
import com.jme3.post.filters.RadialBlurFilter

/**
 * User: ceyhun
 * Date: 2/13/13
 * Time: 3:36 AM
 * 
 */
object GameEngine extends SimpleApplication {

  import Initialization._
  import ColorRGBA._
  import Vector3f._
  import FastMath._

  lazy val skeleton = new GameSkeleton {

    override val terrainWidth: Int = gameSettings("terrain.width").toInt
    override val terrainHeight: Int = gameSettings("terrain.height").toInt
    override val initialSnakeLength: Int = gameSettings("snake.initlen").toInt
    override val initialBerryCount: Int = gameSettings("game.initberrycount").toInt
    override val diminishInterval: Float = gameSettings("game.diminishfactor").toFloat
    override val terrainSpaceDensity: Float = gameSettings("terrain.spaceDensity").toFloat
    override val dashFactor: Float = gameSettings("game.dashfactor").toFloat
    override val minInterval: Float = gameSettings("game.mintimeperblock").toFloat
    interval = gameSettings("game.timeperblock").toFloat

  }

  private val blockWidth: Float = 1f
  private val berryRadius: Float = blockWidth/5
  private val snakePartRadius: Float = blockWidth/2
  private val fontSize: Float = 25
  implicit val snakeHeight: Float = blockWidth/5
  private var miniMapRatio = 1f
  private val snakePartFaces: Int = 12
  private val berryFaces: Int = 8
  private lazy val dashEffectEnabled = gameSettings("effects.dash.enabled") > 0


  private var chaseCam: ChaseCamera = _
  private var birdViewCam: Camera = _
  private var dashFilter: RadialBlurFilter = _


  override def simpleInitApp() {
    settings setTitle "SnakeMazia!"
    gameSettings.init()

    setUpCamera()
    setLighting()
    setUpSkyDome()

    setUpGameObjects()
    setUpGameEngineEventListeners()

    setUpGui()

    initInputs()

    skeleton.gameState = Paused
  }

  object gameSettings extends Properties {

    def init() {
      import resource._
      val gameSettings = new Properties()

      try for {
        defaultProp <- managed(getClass.getResourceAsStream("default.properties"))
      } load(defaultProp) catch { case _: Throwable => {} }

      try for {
        userProp <- managed(new FileInputStream("properties.txt"))
      }  gameSettings load userProp catch { case _: Throwable => {}}

    }

    def apply(key: String): Double = {
      getProperty(key).toDouble
    }

  }

  val nodeMap = MMap.empty[String, MMap[Locatable, Node]]

  override def simpleUpdate(tpf: Float) {
    skeleton.update(tpf)

    /**
     * Update animations
     */
    val alpha = skeleton.animationAlpha

    //update snake local transformation
    var i = 0
    skeleton.snake.parts zip skeleton.snake.nextDirections foreach { case (p, nextDir) =>
      val localNode = getLocalTransformationNode(snakeNode, p)
      val up = UNIT_Y
      val ahead: Vector3f = p.dir
      localNode.setLocalTranslation(ahead.mult(blockWidth*alpha))
      localNode.getLocalRotation.lookAt(ahead, up)
      val q = new Quaternion()
      q.lookAt(nextDir, up)
      localNode.getLocalRotation.slerp(q, alpha)


      val partNode = localNode.getChild(0)
      val sign = if (i % 2 == 0) 1 else -1
      partNode.setLocalTranslation(UNIT_X.mult(blockWidth*0.05f*sign*sin(TWO_PI*alpha)))
      i += 1
    }

    //update berries local transformation
    skeleton.berries foreach { b =>
      val node = getLocalTransformationNode(berriesNode, b)
      val a = DEG_TO_RAD*180*cos(2*b.animationValue)*sin(b.animationValue)
      node.setLocalRotation(new Quaternion().fromAngles(-2*a, a, a/5))
    }
  }

  def putNode(parent: Node, globalTrans: Node, l: Locatable) {
    //get the actual spatial,
    val actualNode = globalTrans.getChild(0)
    //remove it from its current parent (global transformation node)
    actualNode removeFromParent()

    //create a new local transformation node
    val localTrans = new Node(s"lt${actualNode.getName}")

    //create hierarchy actual <- local <- global
    localTrans attachChild actualNode
    globalTrans attachChild localTrans
    parent attachChild globalTrans

    //save global transformation node
    nodeMap(parent.getName)(l) = globalTrans
  }
  def removeNode(parent: Node, l: Locatable): Boolean = getGlobalTransformationNode(parent, l).removeFromParent()

  def getLocalTransformationNode(n: Node, l: Locatable): Node = nodeMap(n.getName)(l).getChild(0).asInstanceOf[Node]
  def getGlobalTransformationNode(n: Node, l: Locatable): Node = nodeMap(n.getName)(l)

  object Initialization {

    def setUpCamera() {

      flyCam setEnabled false
      // Enable a chase cam for this target (typically the player).
      chaseCam = new ChaseCamera(cam, getLocalTransformationNode(snakeNode, SnakeHead), inputManager)
      chaseCam setDefaultDistance snakeHeight*80
      chaseCam setMaxDistance snakeHeight*200
      chaseCam setMinDistance snakeHeight*40
      chaseCam setTrailingSensitivity 10
      chaseCam setEnabled true
      chaseCam setSmoothMotion true
      val processor = new FilterPostProcessor(assetManager)
      dashFilter =  new RadialBlurFilter(1, sqrt(1/skeleton.interval))
      dashFilter.setEnabled(false)
      processor.addFilter(dashFilter)
      renderManager.getMainView("Default").addProcessor(processor)


      birdViewCam = cam.clone
      val aspect = birdViewCam.getWidth.toFloat / birdViewCam.getHeight
      birdViewCam.setViewPort(1-miniMapRatio, 1f, 1-miniMapRatio, 1f)
      birdViewCam.setFrame(ZERO.setElev(100).subtract(blockWidth/2, 0, blockWidth/2), UNIT_X, UNIT_Z, UNIT_Y.negate())
      birdViewCam lookAtDirection(UNIT_Y.negate(), UNIT_Z)
      val frustumSize = (skeleton.terrainWidth max skeleton.terrainHeight)*blockWidth/2
      birdViewCam.setFrustum(-1000, 1000, -aspect * frustumSize, aspect * frustumSize, frustumSize, -frustumSize)
      birdViewCam.setParallelProjection(true)

      val birdViewPort = renderManager.createPostView("BirdView Cam", birdViewCam)
      birdViewPort.setClearFlags(true, true, true)
      birdViewPort.attachScene(gameObjectsNode)
      birdViewPort.attachScene(guiNode)

    }

    def setUpGameObjects() {
      gameObjectsNode
    }

    def setLighting() {
      List((1, -1, -2), (-1, -2, 2), (2, -1, 1)) foreach { vector =>
        val sun = new DirectionalLight
        sun setDirection vector.normalizeLocal()
        sun setColor White
        rootNode addLight sun
      }
    }

    def setUpSkyDome() {
      implicit def tName(s: Symbol) =
        assetManager loadTexture s"Textures/Interstellar/interstellar_${s.name}.tga"

      rootNode attachChild SkyFactory.createSky(assetManager,'ft, 'bk,  'lf, 'rt, 'up, 'dn, 1.divideLocal(10))
    }

    lazy val gameObjectsNode = {
      val gameObjectsNode = new Node("Game Objects' root")
      gameObjectsNode setCullHint Spatial.CullHint.Never
      gameObjectsNode setLocalTranslation(-skeleton.terrainWidth*blockWidth/2, 0, -skeleton.terrainHeight*blockWidth/2)

      gameObjectsNode attachChild terrainNode
      gameObjectsNode attachChild snakeNode
      gameObjectsNode attachChild berriesNode

      rootNode attachChild gameObjectsNode
      gameObjectsNode
    }


    def setUpGameEngineEventListeners() {
      //when snake moves
      skeleton.snakePartMovedHandlers += { s =>
        s.parts.foreach { sp =>
          getGlobalTransformationNode(snakeNode, sp).setLocalTranslation(awtPoint2Point3f(sp.point))
        }
      }
      //when a snake part added
      skeleton.snakePartAddedHandlers += { sps =>
        sps foreach { createSnakePartNode(_) }
        dashFilter.setSampleStrength(sqrt(1/skeleton.interval))
      }

      //when berries added
      skeleton.berryAddedHandlers += { bs =>
        bs foreach { createBerryNode(_) }
      }

      //when berries removed
      skeleton.berryRemovedHandlers += { bs =>
        bs foreach { removeNode(berriesNode, _) }
      }

      //when game state changes
      skeleton.gameStateHandlers += { newState =>
        if (newState == Paused) {
          pauseTextNode.setCullHint(Spatial.CullHint.Inherit)
        } else if (newState == Continues) {
          pauseTextNode.setCullHint(Spatial.CullHint.Always)
        } else if (newState == Died) {
          pauseTextNode.setCullHint(Spatial.CullHint.Always)
          deathTextNode.setCullHint(Spatial.CullHint.Inherit)
          chaseCam setEnabled false
          flyCam setEnabled true
        }
      }

      //when score changes
      skeleton.scoreChangedHandlers += { newScore =>
        scoreTextNode.setText(f"$newScore%04d")
      }

    }

    def initInputs() {
      mouseInput setCursorVisible false

      object Events extends Enumeration  {
        type Events = Value
        val LEFT, RIGHT, PAUSE, DASH, MAP_CHANGE = Value
      }
      import Events._
      implicit def asString(e: Events) = e.toString

      inputManager addMapping (MAP_CHANGE, new KeyTrigger(KeyInput.KEY_M))
      inputManager addMapping (PAUSE, new KeyTrigger(KeyInput.KEY_P))
      inputManager addMapping (LEFT,  new KeyTrigger(KeyInput.KEY_LEFT))
      inputManager addMapping (RIGHT, new KeyTrigger(KeyInput.KEY_RIGHT))
      inputManager addMapping (DASH,  new KeyTrigger(KeyInput.KEY_SPACE),
                                        new KeyTrigger(KeyInput.KEY_UP),
                                        new MouseButtonTrigger(MouseInput.BUTTON_LEFT))

      val listener = new ActionListener {
        def onAction(name: String, pressed: Boolean, tpf: Float) {
          withName(name) match {
            case MAP_CHANGE => {
              if (pressed) {
                val isBirdView = miniMapRatio < 1
                miniMapRatio = if (isBirdView) 1f else 0.2f
                rootNode setCullHint (if (isBirdView) Spatial.CullHint.Always else Spatial.CullHint.Inherit)
                birdViewCam.setViewPort(1-miniMapRatio, 1f, 1-miniMapRatio, 1f)
              }
            }
            case DASH  => {
              if (skeleton.gameState == Continues) {
                skeleton.isDashing = pressed
                dashFilter setEnabled pressed && dashEffectEnabled
              }
            }
            case PAUSE =>
              if (pressed) {
                if (skeleton.gameState == Continues) {
                  skeleton.gameState = Paused
                } else if (skeleton.gameState == Paused) {
                  skeleton.gameState = Continues
                }
              }
            case LEFT  => if (pressed) skeleton.turnLeft()
            case RIGHT => if (pressed) skeleton.turnRight()
          }
        }
      }

      inputManager addListener(listener, PAUSE, LEFT, RIGHT, DASH, MAP_CHANGE)

    }

    implicit lazy val font = assetManager loadFont "Interface/Fonts/Bangers.fnt"
    lazy val deathFont = assetManager loadFont "Interface/Fonts/FreckleFace.fnt"
    lazy val scoreFont = assetManager loadFont "Interface/Fonts/Michroma.fnt"

    def createTextNode(text: String, color: ColorRGBA = White, size: Float = fontSize, centered: Boolean = true)
                      (implicit font: BitmapFont): BitmapText = {
      val n = new BitmapText(font, !centered)
      n setText text
      n setSize size//guiFont.getCharSet.getRenderedSize
      if (centered) {
        n setLocalTranslation ((settings.getWidth - n.getLineWidth)/2, (settings.getHeight - n.getHeight)/2, 0)
      }
      n setQueueBucket RenderQueue.Bucket.Gui
      n setColor color
      n
    }

    def setUpGui() {
      guiNode attachChild deathTextNode
      guiNode attachChild pauseTextNode
      guiNode attachChild scoreTextNode
    }

    lazy val deathTextNode: BitmapText = {
      //TODO Position
      val n = createTextNode("GAME OVER", Red, 100)(deathFont)
      n setCullHint Spatial.CullHint.Always
      n
    }
    lazy val pauseTextNode: BitmapText = {
      //TODO Position
      val n = createTextNode("Paused", Yellow, 60)
      n setCullHint Spatial.CullHint.Always
      n
    }
    lazy val scoreTextNode: BitmapText = {
      val n = createTextNode(f"${0}%04d", Blue, 30, centered = false)(scoreFont)
      n setLocalTranslation (30, settings.getHeight - n.getLineHeight, 0) // settings.getHeight
      n
    }

    lazy val terrainNode: Node = {
      val terrainNode = new Node("terrainRoot")
      implicit val negHeight: Float = -blockWidth/2

      for { p <- skeleton.terrain.nonSpaces } {

        val halfWidth = blockWidth/2.3f
        val center = awtPoint2Point3f(p)(negHeight) //add -halfWidth

        val box = new Box(center, halfWidth, halfWidth, halfWidth)
        box setStatic()
        TangentBinormalGenerator generate box
        val blockNode = new Geometry(s"block@$p", box)
        blockNode setMaterial (assetManager loadMaterial "Textures/AgedStone/Material.j3m")
//        blockNode setMaterial (assetManager loadMaterial "Textures/Box2/Material.j3m")

        terrainNode attachChild blockNode
      }
      terrainNode
    }

    lazy val snakeNode: Node = {
      val snakeNode = new Node("snake")
      val snakeMap = MMap.empty[Locatable, Node]
      nodeMap("snake") = snakeMap
      skeleton.snake.parts foreach { createSnakePartNode(_, snakeNode) }
      snakeNode
    }

    lazy val berriesNode: Node = {
      val berriesNode = new Node("berries")
      val berriesMap = MMap.empty[Locatable, Node]
      nodeMap(berriesNode.getName) = berriesMap

      skeleton.berries foreach { createBerryNode(_, berriesNode) }
      berriesNode
    }

    def createSnakePartNode(sp: SnakePart, snakeNode: Node = snakeNode) {
      //set appearance
      val isHead = sp == SnakeHead

      val shiny_rock = new Geometry(s"snake_$sp", snakeRock)
      val mat_lit = assetManager loadMaterial (
//        if (isHead) "Textures/Pond/Pond.j3m"
      if (isHead) "Textures/Snake/Head.j3m"
      else        "Textures/Snake/Rest.j3m")

      if (!isHead) {
        mat_lit setColor ("Specular", randomColor())
      }
      shiny_rock setMaterial mat_lit

      //place globally
      val node = new Node(s"trans$sp")
      node setLocalTranslation sp.point
      if (sp == SnakeHead) {
        shiny_rock setLocalScale 1.3f
      }
      node attachChild shiny_rock

      putNode(snakeNode, node, sp)
    }

    def createBerryNode(berry: Berry, berriesNode: Node = berriesNode) {
      val shiny_rock = new Geometry(berry.toString, berryRock)
      val mat = assetManager loadMaterial (berry match {
        case _: HeartBerry => "Textures/Berry/Heart.j3m"
        case _             => "Textures/Berry/Blue.j3m"
      })
      shiny_rock setMaterial mat

      //CCB_CALCULATE CALCULATEPRICE
      //place globally
      val trans = new Node(s"trans$berry")
      trans setLocalTranslation berry.point//.setElevP(berryRadius)
      trans attachChild shiny_rock

      putNode(berriesNode, trans, berry)
    }

    lazy val snakeRock = rock(snakePartRadius)(snakePartFaces)
    lazy val berryRock = rock(berryRadius)(berryFaces)

    def rock(r: Float)(numSphereFaces: Int = 4) = {
      val rock = new Sphere(numSphereFaces, numSphereFaces, r)
      TangentBinormalGenerator generate rock           // for lighting effect
      rock setTextureMode Sphere.TextureMode.Projected // better quality on spheres
      rock
    }

    implicit def awtPoint2Point3f(p: Point)(implicit height: Float) = new Vector3f(blockWidth * p.x, height, blockWidth * p.y)
    implicit def vector3f2AwtPoint(p: Vector3f): Point = new Point((p.x / blockWidth).toInt, (p.z / blockWidth).toInt)

    implicit def vector3ToVector3f[T](t: (T, T, T))(implicit n: Numeric[T]): Vector3f = {
      import n._
      new Vector3f(t._1.toFloat(), t._2.toFloat(), t._3.toFloat())
    }

    implicit def singleToVector3f[T](f: T)(implicit n: Numeric[T]): Vector3f = {
      import n._
      new Vector3f(f.toFloat(), f.toFloat(), f.toFloat())
    }

    implicit class ElevatableVector3f(p: Vector3f) {
      def setElev(implicit elev: Float): Vector3f = p.clone().setY(elev)
    }

    implicit class ElevatablePoint(p: Point) {
      def setElevP(implicit elev: Float): Vector3f = awtPoint2Point3f(p)(elev)
    }

    implicit def direction2Vector3f(dir: Direction): Vector3f = (dir.dx, 0, dir.dy)

  }
  def main(args: Array[String]) {
    start()
  }

}

