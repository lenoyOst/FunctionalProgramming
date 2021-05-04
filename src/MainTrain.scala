import java.util.concurrent.{Executors, ThreadPoolExecutor}

object MainTrain {

  // a simple test for time series
  def testTimeSeries(): Unit = {
    val ts = new TimeSeries("train.csv")
    val a = Vector(1, 2, 3, 4, 5)
    val a1 = Vector(1, 2)
    val a2 = Vector(3, 4, 5)

    val tsp = ts.split(2) // splits the time series in half

    if (!ts.getValues("A").get.sameElements(a))
      println("problem with getValues (-2)")

    if (!tsp(0).getValues("A").get.sameElements(a1)) {
      tsp.foreach((t : TimeSeries) => println(t.getValues("A")))
      println("problem with the first half of the split (-2)")
    }

    if (!tsp(1).getValues("A").get.sameElements(a2))
      println("problem with the second half of the split (-3)")
  }

  /* EntropyAnomalyDetector test
  def testEAD(ts: TimeSeries, chunks: Int): Unit = {
    val es = Executors.newFixedThreadPool(chunks)
    val ac = Thread.activeCount()
    val v = EntropyAnomalyDetector.detect(ts, es, chunks)
    if (Thread.activeCount() <= ac)
      println("wrong number of opened threads (-7)")
    es.shutdown()
    if (!(v(0).feature == "A" && v(0).timeStep == 24))
      println("wrong anomaly reported for feature A with " + chunks + " chunks (-8)")
    if (!(v(1).feature == "B" && v(1).timeStep == 29))
      println("wrong anomaly reported for feature B with " + chunks + " chunks (-8)")
    if (!(v(2).feature == "C" && v(2).timeStep == 50))
      println("wrong anomaly reported for feature C with " + chunks + " chunks (-8)")
  }*/
  def main(args: Array[String]): Unit = {
    testTimeSeries()
    //val ts = new TimeSeries("train2.csv")
    //testEAD(ts, 1)
    //testEAD(ts, 3)
    //testEAD(ts, 5)
    println("done")
  }
}
