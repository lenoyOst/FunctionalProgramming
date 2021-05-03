
object SumSqrAnomalyDetector extends  AnomalyDetector {
  def dist(p1: Point, p2: Point): Double = {
    math.pow(math.sqrt(math.pow((p1.x - p2.x), 2) + math.pow(p1.y - p2.y, 2)),2)
  }
  override def learn(normal: TimeSeries): Map[String, String] = {
      def loopMap(map: Map[String, String], index: Int): Map[String, String] = {
        if (index + 1 < normal.features.length) {
          val tuple3 = Util.findCo(normal, index)
          if (tuple3._3 < 0.9) {
            loopMap(map ++ Map(tuple3._1 -> ("no correlation")), index + 1)
          }
          else {
            val vec = normal.getValues(tuple3._1).get.zipWithIndex.map((x: (Double, Int)) => (new Point(x._1, normal.getValues(tuple3._2).get(x._2))))
            val distList = vec.map((p: Point) => {
              var sum = 0.0
              vec.foreach(sum += dist(p, _))
              sum
            }).toList

            //key : name of first f
            //value : name of f2 , max dist , index
            val max = Util.max(distList.zipWithIndex, (d1: (Double, Int), d2: (Double, Int)) => math.floor(d1._1 - d2._1).toInt)
            val m = map ++ Map((tuple3._1 -> (tuple3._2 + "," + max._1 + "," + max._2)))
            loopMap(m, index + 1)
          }
        }
        else {
          map
        }
      }
        loopMap(Map(), 0)
      }
  def detect_sumS(feature : String , cFeature : String , maxDist : Double , test:TimeSeries):Vector[(String , Int)]  = {
    val points = test.getValues(feature).get.zipWithIndex.map((x: (Double, Int)) => (new Point(x._1, test.getValues(cFeature).get(x._2))))
    var exceptionVector = Vector[(String , Int)]()
    val distList = points.map((p: Point) => {
      var sum = 0.0
      points.foreach(sum += dist(p, _))
      sum
    }).toList
    distList.zipWithIndex.foreach((dist: (Double, Int)) => {
      if(dist._1 > maxDist) exceptionVector = exceptionVector:+ (feature+","+cFeature,dist._2)
    })
    exceptionVector
  }
    override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] =
    {
      def go(vec: Vector[(String, Int)], index: Int): Vector[(String, Int)] = {
        if (index+1 < test.features.length) {
          if(model.get(test.features(index)).get.split(",")(0).compareTo("no correlation") != 0) {
            val m = model.get(test.features(index)).get.split(",")
            go(vec ++ detect_sumS(test.features(index),m(0) ,m(1).toDouble,test), index + 1)
          }
            else {
            go(vec, index + 1)
          }
        }
        else {
          vec
        }
      }
      go(Vector(), 0)
    }
}
