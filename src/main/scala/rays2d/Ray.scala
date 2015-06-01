package rays2d

case class World(objs: Vector[WorldPiece])
trait WorldPiece {
	def dist(ray: Ray): Option[Double]
	def reflect(ray: Ray): Option[Either[(Ray, Color => Color),Color]]
}

case class Point(world: World, x: Double, y: Double)
case class Ray(point: Point, angle: Angle) {
	def next: Option[(Ray, Option[Color] => Color,Double)] = {
		val x = point.world.objs.flatMap(x => x.dist(this).map(y => (y,x)))
		if (x.isEmpty) {
			None
		}
		else {
			val (dist, obj) = x.minBy(_._1)
			val (ray, c) = obj.reflect(this).get
			Some((ray, c, dist))
		}
	}
}
object EmptyWorld extends World(Vector())
object NullRay extends Ray(Point(EmptyWorld, 0, 0), Angle(0))

case class XAxis(color: Option[Color] => Color) extends WorldPiece {
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
	def reflect(rayr Ray): Option[(Ray, Option[Color] => Color)] = {
		if (ray.point.x == 0) {
			None
		}
		else {
			val b = ray.point.y
			if (b > 0) {
				if (ray.angle.theta <= math.Pi) None
				else Some((Angle(2*math.pi - ray.angle.theta), color))
			}
			else if (ray.angle.theta >= math.Pi) None
			else Some((Angle(2*math.pi - ray.angle.theta), color))
		}
	}
}

case class Circle(color: Option[Color] => Color, radius: Double) extends WorldPiece {
	def dist(ray: Ray): Option[Double] = {
		val p = math.hypot(ray.point.x, ray.point.y)
		val maxAngleDeviation = Angle(math.asin(radius / p))
		val angleToCenter = Angle(math.atan2(ray.point.y, ray.point.x))
		if (ray.angle.inInterval(angleToCenter - maxAngleDeviation, angleToCenter + maxAngleDeviation))
			return None
		val phi = ray.angle - angleToCenter
		val sintheta = math.sin(phi.theta)
		val sgn = if (p > radius) 1 else -1
		val x = p*math.cos(phi.theta) + sgn * math.sqrt(radius*radius - p*p*sintheta*sintheta)
		return Some(-x)
	}
	def reflect(ray: Ray): Option[(Ray, Option[Color] => Color)] = {
		val p = math.hypot(ray.point.x, ray.point.y)
		val maxAngleDeviation = Angle(math.asin(radius / p))
		val angleToCenter = Angle(math.atan2(ray.point.y, ray.point.x))
		if (ray.angle.inInterval(angleToCenter - maxAngleDeviation, angleToCenter + maxAngleDeviation))
			return None
		val phi = ray.angle - angleToCenter
		val sintheta = math.sin(phi.theta)
		val sgn = if (p > radius) -1 else 1
		val dx = -p*math.cos(phi.theta) + sgn * math.sqrt(radius*radius - p*p*sintheta*sintheta)
		val c = ray.point.x + dx*math.cos(ray.angle.theta)
		val d = ray.point.y + dx*math.sin(ray.angle.theta)
		val x2 = d / x * (ray.point.y + c*c - d*d)
		val b = math.atan(d - y, c - x2)
		val a = b - ray.angle.theta
		return Some((Angle(a+b), color))
	}
}

case class NoReflect(color: Ray => Color, wp: WorldPiece) extends WorldPiece {
	def dist(ray: Ray): Option[Double] = wp.dist(ray)
	def reflect(ray: Ray): Option[(Ray, Option[Color] => Color)] = {
		wp.reflect(ray) match {
			case None => None
			case Some(_) => Some(NullRay, x => color(ray))
		}
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
