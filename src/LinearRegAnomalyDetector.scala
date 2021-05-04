import SumSqrAnomalyDetector.learn

object LinearRegAnomalyDetector extends AnomalyDetector {
        override def learn(normal: TimeSeries): Map[String, String] = {
                def loopMap(map: Map[String, String], index: Int): Map[String, String] = {
                        if (index+1 < normal.features.length) {
                          val tuple3 =  Util.findCo(normal,index)
                          if(tuple3._3 < 0.9) {
                            loopMap(map ++ Map(tuple3._1  -> ("no correlation")), index + 1)
                          }
                          else{
                          val vec = normal.getValues(tuple3._1).get.zipWithIndex.map((x: (Double,Int)) => (new Point(x._1, normal.getValues(tuple3._2).get(x._2))))
                          val rLine = new Line(vec.toArray)
                          //key : name of first f
                          //value : name of f2 ,a,b, max dist , index
                          val vecN = vec.map((p:Point) => rLine.dist(p))
                          val max = Util.max(vecN.zipWithIndex.toList ,(pointA: (Double,Int) ,pointB:(Double,Int)) => math.floor(pointA._1 - pointB._1).toInt)
                          loopMap(map ++ Map((tuple3._1  -> (tuple3._2+","+rLine.a+","+rLine.b +","+ max._1+ "," + max._2))), index + 1)
                        }}
                        else {
                                map
                        }
                }
                loopMap(Map(), 0)
        }
  def detect_regressionLine(feature : String , cFeature : String,a:Double , b :Double , maxDist : Double , test:TimeSeries):Vector[(String , Int)]  = {
    val points = test.getValues(feature).get.zipWithIndex.map((x: (Double, Int)) => (new Point(x._1, test.getValues(cFeature).get(x._2))))
    val line = new Line(points.toArray)
    line.a(a)
    line.b(b)
    var exceptionVector = Vector[(String , Int)]()
    points.zipWithIndex.foreach((p : (Point,Int)) => if(line.dist(p._1) > maxDist){
      exceptionVector = exceptionVector :+ ((feature+","+cFeature) ,p._2)
    })
    exceptionVector
  }
  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] =
  {
    def go(vec : Vector[(String ,Int)] , index : Int):Vector[(String,Int)] = {
      if(index+1 < test.features.length)
      {
        if(model.get(test.features(index)).get.split(",")(0).compareTo("no correlation") == 0) {
            go(vec , index+1)
        }
        else {
          go(vec ++ detect_regressionLine(test.features(index),model.get(test.features(index)).get.split(",")(0) , model.get(test.features(index)).get.split(",")(1).toDouble , model.get(test.features(index)).get.split(",")(2).toDouble ,model.get(test.features(index)).get.split(",")(3).toDouble,test), index + 1)
        }
      }
      else
      {
        vec
      }
    }
    go(Vector() , 0)
  }
}
