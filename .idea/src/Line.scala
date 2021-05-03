import Util.{mu,variance,map}
class Line(ps: Array[Point] ) {
    private val x: Array[Double] = map(ps.toList, (point: Point) => point.x, (d: Double) => d).toArray
    private val y: Array[Double] = map(ps.toList, (point: Point) => point.y, (d: Double) => d).toArray
    private var _a: Double = mu(map(ps.toList, (point: Point) => (point.x - mu(x)) * (point.y - mu(y)), (d: Double) => d).toArray) / variance(x)
    private var _b: Double = y.sum / y.length - (_a * x.sum / x.length)


    def f(x: Double): Double = {
      x * a + b
    }

    def dist(point: Point): Double = {
      var dis: Double = (-_a * point.x + point.y - _b) / math.sqrt(_a * _a + 1)
      if (dis < 0) dis = -dis
      dis
    }

    def a = _a

    def b = _b

    def a(a:Double):Int =
    {
      _a = a
      0
    }

  def b(b:Double):Int =
  {
    _b = b
    0
  }
  }

