object EntropyAnomalyDetector extends ParAnomalyDetector{

  override def map(ts: TimeSeries):Reports={
    def go(reports: Reports, index:Int):Reports={
      if(index < ts.features.size){
        val values = ts.getValues(ts.features(index)).get
        val ent = Util.entropy(values.toArray)
        val rep = values.zipWithIndex.map((value : (Double,Int))=> (ent - Util.entropy(Util.removeByIndex(values,value._2).toArray) , value._2))
        go(reports :+ Report(ts.features(index) , rep.max._2 , rep.max._1) , index+1)
      }
      else{
        reports
      }
    }
    go(new Reports() , 0)
  }

  override def reduce(r1:Reports,r2:Reports):Reports={
    def go(reports: Reports , index:Int):Reports ={
      if(index < r1.size){
        if(r1(index).anomalyScore >= r2(index).anomalyScore){
          go(reports :+r1(index) , index+1)
        }
        else{
          go(reports :+ r2(index) , index+1)
        }
      }
      else{
        reports
      }
    }
    go(new Reports() , 0)
  }
}