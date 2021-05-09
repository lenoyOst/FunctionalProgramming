import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer




case class Report(feature: String, var timeStep: Int, anomalyScore: Double)

trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]

  def map(ts: TimeSeries): Reports

  def reduce(r1: Reports, r2: Reports): Reports

  // implement
  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val t = ts.split(chunks)
    def go(vector : Vector[Future[Reports]] , index : Int): Vector[Future[Reports]] = {
      if(index < chunks)
        {
          go(vector :+ es.submit[Reports](new Callable[Reports] {
            def call(): Reports = {
              EntropyAnomalyDetector.map(t(index)).map((report : (Report))=>Report(report.feature,(report.timeStep+((ts.length()/chunks)*index)),report.anomalyScore))
            }
          }) , index+1)
        }
      else {
        vector
      }
    }
    val reportsF = go(Vector() , 0)
    def combine(reports: Reports , index : Int):Reports = {
      if(reports.isEmpty) {
        while(!reportsF(index).isDone){}
        combine(reportsF(index).get() , index+1)
      }
      else if(index < chunks) {
        while(!reportsF(index).isDone){}
        combine(EntropyAnomalyDetector.reduce(reports,reportsF(index).get()) , index+1)
      }
      else
        reports
    }
    combine(new Reports() , 0).toVector
  }
}


