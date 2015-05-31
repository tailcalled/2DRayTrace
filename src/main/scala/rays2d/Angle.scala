package rays2d

import scala.annotation.tailrec

object Angle {
	def apply(theta: Double) = {
		new Angle(normalize(theta))
	}
	@tailrec def normalize(theta: Double): Double = {
		val tau = 2*math.Pi
		if (theta < 0) normalize(theta + tau)
		else if (theta >= tau) normalize(theta - tau)
		else theta
	}
}
class Angle private(val theta: Double) extends AnyVal {
	def +(that: Angle) = Angle(this.theta + that.theta)
}
