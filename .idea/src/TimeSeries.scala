import scala.io.Source
import scala.collection.mutable

class TimeSeries(csvFileName:String) {
  var mapFuatures = mutable.Map[String , Vector[Double]]()
  val reader = Source.fromFile(csvFileName).bufferedReader()
  val features= reader.readLine().split(",").toVector
  mapFuatures += ("" -> Vector())
  Util.Flipmatrix(Source.fromFile(csvFileName).getLines().map((s : String)=>s.split(",")).toArray).foreach{(array : Array[String])=>
    val a : Array[Double] = (array.drop(1).map((s:String) =>s.toDouble))
    mapFuatures += ( array(0) -> a.toVector)
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
  def getValues(feature:String,r:Range):Option[Vector[Double]]=
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
