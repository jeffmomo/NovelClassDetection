import Helpers._
import RecordReader._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by jeffmo on 16/10/16.
  */




case class Cluster(radius: Double, centroid: RecordData, label: Label)(implicit metric:DistanceMetric) {

  def contains(record: RecordData)(implicit outth: OUTTH): Boolean = {
    val w = weight(record)
    return w >= outth.value
  }

  def weight(record: RecordData): Double = {
    val dist = distance(record)

    val out = Math.exp(radius - dist)
    out
  }


  def distance(record: RecordData): Double = {
    val dist = metric.distance(centroid, record)
    dist
    //if(dist > radius) dist else -1
  }
}





class Centroid(initial: Record) {

  var mean = initial.data
  val dimensions = initial.data.length

  val records = new ArrayBuffer[Record]()

  def addRecord(record:Record): Unit = {
    records += record
  }

  def clearRecords(): Unit = {
    records.clear()
  }

  def calculateMean(): RecordData = {



    val output = Array.fill[Double](dimensions)(0)

    if(records.isEmpty)
      return output

    records.foreach((a) => {
      for(i <- a.data.indices)
        output(i) += a.data(i)
    })

    mean = output.map(_ / records.length)
    mean
  }

}




object Clusterers {

  private def assignRecordToCentroid(record: Record, centroids: Array[Centroid], metric:DistanceMetric): Int = {

    if(centroids.length == 0)
      println("wtf")
    assert(centroids.length > 0)

    val distances = centroids.map((a) => metric.distance(a.mean, record.data))

    var maxIdx = 0
    var maxVal = distances(0)

    for(i <- 1 until distances.length) {
      if(distances(i) > maxVal) {
        maxIdx = i
        maxVal = distances(i)
      }
    }

    centroids(maxIdx).addRecord(record)
    maxIdx
  }

  def KMeans(records: Array[Record], k: Int, maxIterations: Int = 5)(implicit distanceMetric: DistanceMetric = EuclideanDistance): Array[Cluster] = {

    assert(records.length > 0)
    val dimension = records(0).data.length

    val fillerFn = Array.fill[Double](dimension)(_)

    val minArray = fillerFn(Double.MaxValue)
    val maxArray = fillerFn(Double.MinValue)

    for (record <- records) {
      for (i <- minArray.indices) {
        minArray(i) = Math.min(minArray(i), record.data(i))
        maxArray(i) = Math.max(maxArray(i), record.data(i))
      }
    }

    val centroids: Array[Centroid] =
      Array.range(0, k) map { _ => new Centroid(Record(Array.range(0, dimension) map { i => Random.nextDouble() * (maxArray(i) + 1) - minArray(i) }, -1)) }

    var iterations = 0
    while(iterations < maxIterations) {
      centroids.foreach((a) => a.clearRecords())
      records.foreach((a) => assignRecordToCentroid(a, centroids, distanceMetric))
      centroids.foreach((a) => a.calculateMean())
      // Maybe add stopping when changes are minimal
      iterations += 1
    }

    return centroids.par.filter((a) => a.records.nonEmpty).map((a) => {
      var maxDist = 0.0
      val map = new mutable.HashMap[Label, Int]()

      a.records.foreach((r) => {
        maxDist = Math.max(maxDist, distanceMetric.distance(a.mean, r.data))
        map.put(r.label, map.getOrElse(r.label, 0) + 1)
      })

      Cluster(maxDist, a.mean, map.maxBy((a) => a._2)._1)
    }).toArray


  }


}
