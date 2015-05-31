package rays2d

object Test extends Engine {

	def title = "2D raytracing test"

	var x = 100.0
	var y = 100.0

	def step() = {
		import Keys._
		if (pressed contains Keys.Left) x -= 4
		if (pressed contains Keys.Right) x += 4
		if (pressed contains Keys.Up) y -= 4
		if (pressed contains Keys.Down) y += 4
		drawCircle((x, y), 10, Color(0, 0, 0))
	}

}