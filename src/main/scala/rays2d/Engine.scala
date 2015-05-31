package rays2d

import java.awt.Frame
import java.awt.{Graphics2D, Graphics}
import java.awt.image.BufferedImage
import javax.swing._

abstract class Engine {

	private var graphics: Graphics2D = null

	def title: String

	def step(): Unit
	def start() = {
		val jf = new JFrame(title)
		val comp = new JComponent() {
			var img = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
			override def paintComponent(gfx: Graphics) = {
				gfx.drawImage(img, 0, 0, null)
			}
		}
		jf.add(comp)
		jf.setExtendedState(Frame.MAXIMIZED_BOTH)
		jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		jf.setVisible(true)
		while (true) {
			val img = new BufferedImage(comp.getWidth max 1, comp.getHeight max 1, BufferedImage.TYPE_INT_ARGB)
			graphics = img.getGraphics.asInstanceOf[Graphics2D]
			step()
			comp.img = img
			comp.repaint()
			Thread.sleep(1000/60 - 10)
		}
	}
	def main(args: Array[String]) = {
		start()
	}

}