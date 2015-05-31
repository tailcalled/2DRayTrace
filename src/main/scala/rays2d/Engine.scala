package rays2d

import java.awt.Frame
import java.awt.{Graphics2D, Graphics}
import java.awt.image.BufferedImage
import javax.swing._

abstract class Engine {

	type Key = Int

	private var gfx: Graphics2D = null

	private var _width = 0.0
	private var _height = 0.0

	private var _keys = Set[Key]()
	object Keys {
		import java.awt.event.KeyEvent._
		val Left = VK_LEFT
		val Right = VK_RIGHT
		val Up = VK_UP
		val Down = VK_DOWN
	}

	def width = _width
	def height = _height
	def pressed = _keys

	def title: String

	def step(): Unit
	def start() = {
		val jf = new JFrame(title)
		val comp = new JComponent() {
			var img = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
			override def paintComponent(gfx: Graphics) = {
				gfx.drawImage(img, 0, 0, null)
			}
			setFocusable(true)
			import java.awt.event._
			addKeyListener(new KeyListener() {
				def keyPressed(ev: KeyEvent) = {
					_keys += ev.getKeyCode
				}
				def keyReleased(ev: KeyEvent) = {
					_keys -= ev.getKeyCode
				}
				def keyTyped(ev: KeyEvent) = {}
			})
		}
		jf.add(comp)
		jf.setExtendedState(Frame.MAXIMIZED_BOTH)
		jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		jf.setVisible(true)
		while (true) {
			_width = comp.getWidth max 1
			_height = comp.getHeight max 1
			val img = new BufferedImage(width.toInt, height.toInt, BufferedImage.TYPE_INT_ARGB)
			gfx = img.getGraphics.asInstanceOf[Graphics2D]
			step()
			comp.img = img
			comp.repaint()
			Thread.sleep(1000/60 - 10)
		}
	}
	def main(args: Array[String]) = {
		start()
	}

	private def setColor(col: Color) = gfx.setColor(new java.awt.Color((col.r * 255).round.toInt, (col.g * 255).round.toInt, (col.b * 255).toInt))
	def drawLine(p1: (Double, Double), p2: (Double, Double), col: Color) = {
		setColor(col)
		gfx.drawLine(p1._1.round.toInt, p1._2.round.toInt, p2._1.round.toInt, p2._2.round.toInt)
	}
	def drawCircle(p: (Double, Double), rad: Double, col: Color) = {
		setColor(col)
		gfx.drawOval((p._1 - rad).round.toInt, (p._2 - rad).round.toInt, (rad * 2).round.toInt, (rad * 2).round.toInt)
	}

}