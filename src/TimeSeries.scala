import scala.io.Source
import scala.collection.mutable

class TimeSeries(csvFileName:String , mapFeatures : Map[String,Vector[Double]] = Map()) {
  var mapFuatures : Map[String,Vector[Double]] = Map()
  var features : Vector[String] = Vector()
  if(mapFeatures.size > 0) {
    mapFuatures = mapFeatures
    features = mapFuatures.keys.toVector
  }
  else {
    val reader = Source.fromFile(csvFileName).bufferedReader()
    features = reader.readLine().split(",").toVector
    mapFuatures = Util.fillMap(Map(), Vector(), 1, 0, Source.fromFile(csvFileName).getLines().map((s: String) => s.split(",")).toList)
  }
  // given name of a feature return in O(1) its value series
  def getValues(feature:String):Option[Vector[Double]]=
    {
      mapFuatures.get(feature)
    }

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature:String,timeStep:Int):Option[Double]=
    {
      if(timeStep>0 && mapFuatures.get(feature)!= None && timeStep < mapFuatures.get(feature).get.length)
      {
        Option((mapFuatures.get(feature).head)(timeStep))
      }
      else {
        None
      }
    }

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]= {
    {
      if(r.start>0 && mapFuatures.get(feature)!= None && (r.end)<(mapFuatures.get(feature).get.length))
      {
        Option(mapFuatures.get(feature).get.slice(r.start , r.end+1))
      }
    else {
        None
      }
    }
  }
  def length():Int= {
    mapFuatures.get(features(0)).get.length
  }

  def split(divition : Int):Array[TimeSeries]=
  {
    def go(size :Int ,chunkSize : Int , splitArray:Array[TimeSeries]):Array[TimeSeries]={
      if((size - (splitArray.size)*chunkSize) < 2*chunkSize) {
        splitArray :+ new TimeSeries("",mapFuatures.map((set : (String,Vector[Double]))=>((set._1 , set._2.drop(splitArray.size*chunkSize)))))
      }
      else{
        go(size , chunkSize,splitArray :+ new TimeSeries("",mapFuatures.map((set : (String,Vector[Double]))=>(set._1 -> set._2.drop(splitArray.size*chunkSize).dropRight(size - ((splitArray.size+1)*chunkSize))))))
      }
    }
    go(this.length() , this.length()  / divition , Array())
  }
}
