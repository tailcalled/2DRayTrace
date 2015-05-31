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
	def -(that: Angle) = Angle(this.theta - that.theta)
	def inInterval(bound1: Angle, bound2: Angle) = {
		if (bound1 == bound2) bound1 == this
		else if (bound1.theta > bound2.theta) this.theta >= bound1.theta || this.theta <= bound2.theta
		else this.theta >= bound1.theta && this.theta <= bound2.theta
	}
}
