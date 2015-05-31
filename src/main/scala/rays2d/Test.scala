package rays2d

object Test extends Engine {

	def title = "2D raytracing test"

	var x = 10.0
	var y = 10.0

	val world = World(Vector(XAxis(null)))

	def step() = {
		import Keys._
		import math._
		if (pressed contains Keys.Left) x -= 0.5
		if (pressed contains Keys.Right) x += 0.5
		if (pressed contains Keys.Up) y -= 0.5
		if (pressed contains Keys.Down) y += 0.5
		val nRays = 10000
		val rays = (0 until nRays).map(ray => Ray(Point(world, x, y), Angle(ray * Pi * 2 / nRays))).toArray
		val ress = rays.map(_.next)
		for (i <- 0 until nRays) {
			val j = (i + 1) % nRays
			(ress(i), ress(j)) match {
				case (Some((_, di)), Some((_, dj))) =>
					val ti = i * Pi * 2 / nRays
					val tj = j * Pi * 2 / nRays
					drawLine((x + cos(ti) * di, y + sin(ti) * di), (x + cos(tj) * dj, y + sin(tj) * dj), Color(0, 0, 0))
				case _ =>
			}
		}
		drawCircle((x, y), 10, Color(0, 0, 0))
	}

}
