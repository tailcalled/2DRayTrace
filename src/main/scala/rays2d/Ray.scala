package rays2d

case class World(objs: Vector[WorldPiece])
trait WorldPiece {
	def dist(ray: Ray): Double
	def reflect(ray: Ray): Either[(Ray, Color => Color),Color]
}

case class Point(world: World, x: Double, y: Double)
case class Ray(point: Point, dir: Double) {
	def next: Option[(Either[(Ray, Color => Color),Color],Double)] = {
		if (point.world.objs.length == 0) {
			return None
		}
		val (dist, obj) = point.world.objs.map(x => (x.dist(this), x)).minBy(_._1)
		return Some((obj.reflect(this), dist))
	}
}


