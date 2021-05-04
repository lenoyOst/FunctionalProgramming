object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    normal.features.map((s: String) => (s -> (Util.max(normal.getValues(s).get.zipWithIndex.map((d: (Double, Int)) => (Util.zscore(normal.getValues(s).get.toArray, d._1).abs, Util.zscore(normal.getValues(s).get.toArray, d._1).abs + "," + d._2)).toList, (d1: (Double, String), d2: (Double, String)) => math.floor(d1._1 - d2._1).toInt)._2))).toMap
  }
  def detect_noCorrelation(feature : String, max : Double , test:TimeSeries):Vector[(String , Int)]  = {
    var vec=Vector[(String , Int)]()
    val array = test.getValues(feature).get.toArray
    test.getValues(feature).get.map((value:Double) => Util.zscore(array, value).abs).zipWithIndex.foreach((zScor:(Double,Int))=>{if(zScor._1 > max){
      vec = vec :+ (feature , zScor._2)
    }})
    vec
  }
  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val testM = learn(test)

    def go(vector: Vector[(String, Int)], index: Int): Vector[(String, Int)] = {
      if (index < test.features.length) {
        go(vector ++ detect_noCorrelation(test.features(index), model.get(test.features(index)).get.split(",")(0).toDouble, test), index + 1)
      }
      else {
        vector
      }
    }

    go(Vector(), 0)
  }
}
