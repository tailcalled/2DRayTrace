package rays2d

object Angle {
	def apply(theta: Double) = {
		new Angle(normalize(theta))
	}
	@tailrec def normalize(theta: Double) = {
		val tau = 2*math.Pi
		if (theta < 0) normalize(theta + tau)
		else if (theta >= tau) normalize(theta - tau)
		else theta
	}
}
case class Angle(theta: Double) extends AnyVal

