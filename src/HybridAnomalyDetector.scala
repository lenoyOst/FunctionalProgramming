

object HybridAnomalyDetector extends  AnomalyDetector {
  def dist(p1: Point, p2: Point): Double = {
    math.pow(math.sqrt(math.pow((p1.x - p2.x), 2) + math.pow(p1.y - p2.y, 2)),2)
  }
  override def learn(normal: TimeSeries): Map[String, String] = {

    def loopMap(map: Map[String, String], index: Int): Map[String, String] = {
      if (index+1 < normal.features.length) {
        val tuple3 = Util.findCo(normal, index)
        if (tuple3._3 > 0.5) {
          val vec = normal.getValues(tuple3._1).get.zipWithIndex.map((x: (Double, Int)) => (new Point(x._1, normal.getValues(tuple3._2).get(x._2))))
          if (tuple3._3 >= 0.9) //regration line
          {
            val rLine = new Line((vec.toArray))
            //key : name of first f
            //value : regression line,a,b , name of f2 , max dist , index
            val max = Util.max(vec.zipWithIndex.toList, (pointA: (Point,Int), pointB: (Point,Int)) => math.floor(rLine.dist(pointA._1) - rLine.dist(pointB._1)).toInt)
            loopMap(map ++ Map((tuple3._1 -> ("regression line,"+rLine.a+","+rLine.b+","+tuple3._2 + "," + rLine.dist(max._1) + "," +max._2))), index + 1)
          }
          else//circle
          {
            val distList = vec.map((p: Point) => {
              var sum = 0.0
              vec.foreach(sum += dist(p, _))
              (p, sum)
            }).toList
            val centerPoint = Util.max(distList, (p1: (Point, Double), p2: (Point, Double)) => math.floor(p2._2 - p1._2).toInt)._1
            //key : name of first f
            //value : circle , name of f2 , max dist
            val max = Util.max(vec.zipWithIndex.map((p: (Point,Int)) => (dist(p._1, centerPoint) , p._2)).toList, (d1: (Double,Int), d2: (Double,Int)) => math.floor(d1._1 - d2._1).toInt)
            loopMap(map ++ Map((tuple3._1 -> ("circle"+"," + tuple3._2 + "," + max._1 + "," + max._2))), index + 1)
          }
        }
        else //no corolation , zScor
        {
          val max = Util.max(normal.getValues(tuple3._1).get.map((v: Double) => Util.zscore(normal.getValues(tuple3._1).get.toArray, v).abs).zipWithIndex.toList, (d1: (Double , Int), d2: (Double,Int)) => math.floor(d1._1 - d2._1).toInt)
          loopMap(map ++ Map(tuple3._1 -> ("no correlation" + "," +max._1 + "," + max._2)) , index + 1)
        }
      }
      else if(index < normal.features.length )
      {
          val max = Util.max(normal.getValues(normal.features(index)).get.map((v: Double) => Util.zscore(normal.getValues(normal.features(index)).get.toArray, v).abs).zipWithIndex.toList, (d1: (Double , Int), d2: (Double,Int)) => math.floor(d1._1 - d2._1).toInt)
          loopMap(map ++ Map(normal.features(index) -> ("no correlation" + "," +max._1 + "," + max._2)) , index + 1)
      }
      else {
        map
      }
    }
    loopMap(Map(), 0)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] =
  {
    def detect_circle(feature : String , cFeature : String, radius : Double):Vector[(String , Int)]  = {
      val points = test.getValues(feature).get.zipWithIndex.map((x: (Double, Int)) => (new Point(x._1, test.getValues(cFeature).get(x._2))))
      val distList = points.map((p: Point) => {
        var sum = 0.0
        points.foreach(sum += dist(p, _))
        (p, sum)
      }).toList
      val centerPoint = Util.max(distList, (p1: (Point, Double), p2: (Point, Double)) => math.floor(p2._2 - p1._2).toInt)._1
      var exceptionVector = Vector[(String , Int)]()
      points.zipWithIndex.foreach((p : (Point,Int)) => if(dist(p._1,centerPoint) > radius){
        exceptionVector = exceptionVector :+ ((feature+","+cFeature) ,p._2)
      })
      exceptionVector
    }
    def go(vec : Vector[(String ,Int)] , index : Int):Vector[(String,Int)] = {
      if(index < test.features.length) {
        if (model.get(test.features(index)).get.split(",")(0).compareTo("no correlation") == 0) {
          go(vec ++ ZAnomalyDetector.detect_noCorrelation(test.features(index) ,model.get(test.features(index)).get.split(",")(1).toDouble , test), index + 1)
        }
        else if (model.get(test.features(index)).get.split(",")(0).compareTo("circle") == 0) {
          go(vec ++ detect_circle(test.features(index) ,model.get(test.features(index)).get.split(",")(1) ,model.get(test.features(index)).get.split(",")(2).toDouble), index + 1)
        }
        else {
          go(vec ++ LinearRegAnomalyDetector.detect_regressionLine(test.features(index) ,model.get(test.features(index)).get.split(",")(3) ,model.get(test.features(index)).get.split(",")(1).toDouble ,model.get(test.features(index)).get.split(",")(2).toDouble ,model.get(test.features(index)).get.split(",")(4).toDouble , test), index + 1)
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
