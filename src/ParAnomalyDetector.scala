import java.util.concurrent.{ExecutorService, Future}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)

trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]
  def map(ts: TimeSeries): Reports
  def reduce(r1: Reports, r2: Reports): Reports

  // implement
  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = ???
}

