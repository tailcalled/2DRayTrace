package rays2d

case class World(objs: Vector[WorldPiece])
trait WorldPiece {
	def dist(ray: Ray): Option[Double]
	def reflect(ray: Ray): Option[Either[(Ray, Color => Color),Color]]
}

case class Point(world: World, x: Double, y: Double)
case class Ray(point: Point, angle: Angle) {
	def next: Option[(Either[(Ray, Color => Color),Color],Double)] = {
		val x = point.world.objs.flatMap(x => x.dist(this).map(y => (y,x)))
		if (x.isEmpty) {
			return None
		}
		val (dist, obj) = x.minBy(_._1)
		return Some((obj.reflect(this).get, dist))
	}
}

case class XAxis(color: Color) extends WorldPiece {
	def dist(ray: Ray): Option[Double] = {
		if (ray.point.x == 0) {
			return None
		}
		val b = ray.point.y
		if (b > 0) {
			if (ray.angle.theta <= math.Pi) return None
			if (ray.angle.theta < 1.5 * math.Pi) return Some(math.hypot(b, b*math.tan(1.5*math.Pi - ray.angle.theta)))
			return Some(math.hypot(b, b*math.tan(ray.angle.theta - 1.5 * math.Pi)))
		}
		if (ray.angle.theta >= math.Pi) return None
		if (ray.angle.theta < 0.5 * math.Pi) return Some(math.hypot(b, b*math.tan(0.5*math.Pi - ray.angle.theta)))
		return Some(math.hypot(b, b*math.tan(ray.angle.theta - 0.5*math.Pi)))
	}
	def reflect(ray: Ray): Option[Either[(Ray, Color => Color),Color]] = {
		if (ray.point.x == 0) {
			return None
		}
		val b = ray.point.y
		if (ray.point.x > 0) {
			if (ray.angle.theta <= math.Pi) return None
			return Some(Right(color))
		}
		if (ray.angle.theta >= math.Pi) return None
		return Some(Right(color))
	}
}


