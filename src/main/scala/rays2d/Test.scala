package rays2d

object Test extends Engine {

	def title = "2D raytracing test"

	var x = 110.0
	var y = 110.0

	def world = World(Vector(
		NoReflect(_ => null, Translated(XAxis(null), (0, 50))),
		NoReflect(_ => null, Translated(XAxis(null), (0, height - 50))),
		NoReflect(_ => null, Translated(Rotated(XAxis(null), Angle(math.Pi / 2)), (50, 0))),
		NoReflect(_ => null, Translated(Rotated(XAxis(null), Angle(math.Pi / 2)), (width - 50, 0))),
		NoReflect(_ => null, Translated(Rotated(Circle(null, 50), Angle(math.Pi / 4)), (500, 500))),
		NoReflect(_ => null, Translated(Circle(null, 50), (50, 50))),
		NoReflect(_ => null, Translated(Circle(null, 50), (500, 50))),
		NoReflect(_ => null, Translated(Circle(null, 50), (700, 700)))
	))

	def step() = {
		import Keys._
		import math._
		if (pressed contains Keys.Left) x -= 4
		if (pressed contains Keys.Right) x += 4
		if (pressed contains Keys.Up) y -= 4
		if (pressed contains Keys.Down) y += 4
		val nRays = 200
		val rays = (0 until nRays).map(ray => Ray(Point(world, x, y), Angle(ray * Pi * 2 / nRays))).toArray
		val ress = rays.map(_.next)
		for (i <- 0 until nRays) {
			val j = (i + 1) % nRays
			(ress(i), ress(j)) match {
				case (Some((_, di1)), Some((_, dj1))) =>
					val di = if (di1 > 500) 500 else di1
					val dj = if (dj1 > 500) 500 else dj1
					val ti = i * Pi * 2 / nRays
					val tj = j * Pi * 2 / nRays
					if ((di != 500 || dj != 500) && (di > 250 || dj > 250)) {
						val k2 = i + i + 1
						val tk = k2 * Pi / nRays
						Ray(Point(world, x, y), Angle(k2 * Pi / nRays)).next match {
							case Some((_, dk1)) =>
								val dk = if (dk1 > 500) 500 else dk1
								drawLine((x + cos(ti) * di, y + sin(ti) * di), (x + cos(tk) * dk, y + sin(tk) * dk), Color(0, 0, 0))
								drawLine((x + cos(tk) * dk, y + sin(tk) * dk), (x + cos(tj) * dj, y + sin(tj) * dj), Color(0, 0, 0))
							case None =>
						}
					}
					else drawLine((x + cos(ti) * di, y + sin(ti) * di), (x + cos(tj) * dj, y + sin(tj) * dj), Color(0, 0, 0))
				case _ =>
			}
		}
		drawCircle((x, y), 10, Color(0, 0, 0))
	}

}
