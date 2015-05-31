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
			None
		}
		else {
			val (dist, obj) = x.minBy(_._1)
			Some((obj.reflect(this).get, dist))
		}
	}
}

case class XAxis(color: Color) extends WorldPiece {
	def dist(ray: Ray): Option[Double] = {
		if (ray.point.x == 0) {
			None
		}
		else {
			val b = ray.point.y
			if (b > 0) {
				if (ray.angle.theta <= math.Pi) None
				else if (ray.angle.theta < 1.5 * math.Pi) Some(math.hypot(b, b*math.tan(1.5*math.Pi - ray.angle.theta)))
				else Some(math.hypot(b, b*math.tan(ray.angle.theta - 1.5 * math.Pi)))
			}
			else if (ray.angle.theta >= math.Pi) None
			else if (ray.angle.theta < 0.5 * math.Pi) Some(math.hypot(b, b*math.tan(0.5*math.Pi - ray.angle.theta)))
			else Some(math.hypot(b, b*math.tan(ray.angle.theta - 0.5*math.Pi)))
		}
	}
	def reflect(ray: Ray): Option[Either[(Ray, Color => Color),Color]] = {
		if (ray.point.x == 0) {
			None
		}
		else {
			val b = ray.point.y
			if (b > 0) {
				if (ray.angle.theta <= math.Pi) None
				else Some(Right(color))
			}
			else if (ray.angle.theta >= math.Pi) None
			else Some(Right(color))
		}
	}
}

case class Circle(color: Color, radius: Double) extends WorldPiece {
	def dist(ray: Ray): Option[Double] = {
		val p = math.hypot(ray.point.x, ray.point.y)
		val maxAngleDeviation = Angle(math.asin(radius / p))
		val angleToCenter = Angle(math.atan2(ray.point.y, ray.point.x))
		if (ray.angle.inInterval(angleToCenter - maxAngleDeviation, angleToCenter + maxAngleDeviation))
			return None
		val phi = ray.angle - angleToCenter
		val sintheta = math.sin(phi.theta)
		val x = p*math.cos(phi.theta) + math.sqrt(radius*radius - p*p*sintheta*sintheta)
		return Some(-x)
	}
	def reflect(ray: Ray): Option[Either[(Ray, Color => Color),Color]] = {
		val p = math.hypot(ray.point.x, ray.point.y)
		val maxAngleDeviation = Angle(math.asin(radius / p))
		val angleToCenter = Angle(math.atan2(ray.point.y, ray.point.x))
		if (ray.angle.inInterval(angleToCenter - maxAngleDeviation, angleToCenter + maxAngleDeviation))
			return None
		return Some(Right(color))
	}
}

case class Rotated(piece: WorldPiece, by: Angle) extends WorldPiece {
	def transform(ray: Ray) = {
		import math._
		val rayPLen = hypot(ray.point.x, ray.point.y)
		val rayPDir = atan2(ray.point.y, ray.point.x)
		val tx = cos(rayPDir + by.theta) * rayPLen
		val ty = sin(rayPDir + by.theta) * rayPLen
		Ray(Point(ray.point.world, tx, ty), ray.angle + by)
	}
	def untransform(ray: Ray) = {
		import math._
		val rayPLen = hypot(ray.point.x, ray.point.y)
		val rayPDir = atan2(ray.point.y, ray.point.x)
		val tx = cos(rayPDir - by.theta) * rayPLen
		val ty = sin(rayPDir - by.theta) * rayPLen
		Ray(Point(ray.point.world, tx, ty), ray.angle - by)
	}
	def dist(ray: Ray) = {
		piece.dist(transform(ray))
	}
	def reflect(ray: Ray) = {
		piece.reflect(transform(ray)) match {
			case None => None
			case Some(Left((ray2, f))) => Some(Left((untransform(ray2), f)))
			case Some(Right(x)) => Some(Right(x))
		}
	}
}

case class Translated(piece: WorldPiece, by: (Double, Double)) extends WorldPiece {
	def transform(ray: Ray) = {
		Ray(Point(ray.point.world, ray.point.x - by._1, ray.point.y - by._2), ray.angle)
	}
	def untransform(ray: Ray) = {
		Ray(Point(ray.point.world, ray.point.x + by._1, ray.point.y + by._2), ray.angle)
	}
	def dist(ray: Ray) = {
		piece.dist(transform(ray))
	}
	def reflect(ray: Ray) = {
		piece.reflect(transform(ray)) match {
			case None => None
			case Some(Left((ray2, f))) => Some(Left((untransform(ray2), f)))
			case Some(Right(x)) => Some(Right(x))
		}
	}
}
