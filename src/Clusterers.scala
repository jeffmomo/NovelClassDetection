import Helpers._
import RecordReader._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by jeffmo on 16/10/16.
  */

class OUTTH(initial: Double = 0.7, epsilon: Double) {

  var value = initial

  def adjustFalseNovel(weight: Double): Unit = {
    assert(weight < value)

    // increase slack space
    if(value - weight < epsilon)
      value -= epsilon
  }


  def adjustFalseExisting(): Unit = {
    value += epsilon
  }


}



case class Cluster(radius: Double, centroid: RecordData, label: Label)(implicit metric:DistanceMetric) {

  def contains(record: RecordData)(implicit outth: OUTTH): Boolean = {
    return weight(record) >= outth.value
  }

  def weight(record: RecordData): Double = {

    val dist = distance(record)

    Math.exp(radius - dist)
  }


  def distance(record: RecordData): Double = {
    val dist = metric.distance(centroid, record)

    if(dist > radius) dist else -1
  }
}


trait Classifier {
  def classify(x: RecordData): Label
}

//class Vertex[T](value: T) {
//
//  val edges = new ArrayBuffer[Vertex[T]]()
//
//  def addEdgeTo(v: Vertex[T]): Unit = {
//    edges.append(v)
//  }
//
//
//
//}

class WeightedGraph[VertexType](initials: Set[VertexType]) {

  val vertices = initials
  val edges = new mutable.HashMap[VertexType, ArrayBuffer[VertexType]]()

  def addEdge(a: VertexType, b: VertexType): Unit = {
    val vertex = edges.getOrElse(a, new ArrayBuffer[VertexType]())
    vertex.append(b)
    edges.put(a, vertex)
  }

  def findConnectedComponents(): Seq[Set[VertexType]] = {

    return null
  }
}

//noinspection SpellCheckingInspection
class NovelClassDetector(ensemble: Ensemble) {



  implicit val metric: DistanceMetric = EuclideanDistance
  implicit val outth:OUTTH = new OUTTH(initial = 0.7, epsilon = 0.001)
  val q: Int = 6
  val numIntervals: Int = 10
  val k:Int = 10;
  val chunkSize: Int = 1000
  val scThreshold: Double = 0.8


  type TupleOf_RecordData_Weight_qSNC = (RecordData, Double, Double)


  def discoverRealOutliers(inputs: Array[RecordData]): Array[RecordData] = {

    val (inlierTuple, f_outlierTuple) =
      inputs
      .map((a) => (a, ensemble.weight(a)))
      .partition((a) => a._2 < 1)

    val (f_outliers, f_outlierWeights) = f_outlierTuple.unzip
    val (inliers, _) = inlierTuple.unzip


    val realOutliers: Array[TupleOf_RecordData_Weight_qSNC] =
      f_outlierTuple
      .map((a) => (a._1, a._2, Helpers.qSNC(q, a._1, f_outliers, inliers)))
      .filter((tuple3) => tuple3._3 >= 0)

    assert(realOutliers.length > 0)

    val minWeight = realOutliers.minBy((a) => a._2)._2


    // no need to sort actually
    val outliersWithNscoreOrdered = realOutliers.map((a) => (a._1, (1 - a._2) / (1 - minWeight))).sortBy((a) => a._2)

    // Binning from min to max? or from 0 to 1
    val minNscore = 0  // outliersWithNscoreOrdered(0)._2
    val maxNscore = 1  // outliersWithNscoreOrdered.last._2
    val interval = (maxNscore - minNscore) / numIntervals

    val bins = Array.fill[Int](numIntervals)(0)

    // need to make sure it is correctly filled
    outliersWithNscoreOrdered.foreach((a) => bins(((a._2 - minNscore) / interval).toInt) += 1)

    val sum = bins.sum
    assert(sum == outliersWithNscoreOrdered.length) // make sure we didnt lose any records



    val probabilities = bins.map((a) => a / sum.toDouble)

    assert(probabilities.sum == 1) // make sure probabilities are correct

    var probSum = 0.0
    val cdf = for(i <- probabilities.indices) yield {
      probSum += probabilities(i)
      probSum
    }

    val gini = Helpers.gini(cdf)
    if(gini > 0) {
      if(gini <= (outliersWithNscoreOrdered.length - 1.0) / (3.0 * outliersWithNscoreOrdered.length)) {
        // make sure this is actually the last interval
        return (outliersWithNscoreOrdered filter ((a) => a._2 >= minNscore + interval)).map(_._1)
      } else {
        return outliersWithNscoreOrdered map (_._1)
      }
    }

    // All items filtered out
    return new Array[RecordData](0)

  }


  def buildGraph(items: Array[RecordData]): Unit = {

    val graph = new WeightedGraph[RecordData](items.toSet)

    val k_v = k * items.length / chunkSize

    // change RecordData to Record with bogus label
    val clusters = Clusterers.KMeans(items.map(Record(_, -1.0)), k_v)

    clusters.foreach((c) => {
      val sortedClusterDistanceTuple = clusters.map((a) => (a, metric.distance(c.centroid, a.centroid))).sortBy(_._2)
      val avgDist = sortedClusterDistanceTuple.map(_._2).sum / (sortedClusterDistanceTuple.length - 1.0) // length - 1 to get rid of the current cluster
      val closestCluster = sortedClusterDistanceTuple(1) // closest at index 1 because index 0 will be the current cluster as distance(a, a) == 0

      val sc = Helpers.silhouetteCoefficient(closestCluster._2, avgDist)

      if(sc < scThreshold) {
        graph.addEdge(c.centroid, closestCluster._1.centroid)
      }

    })

    val connectedComponentCentroidDistance = graph.findConnectedComponents().map((a) => {

      // make sure the connected compnents have at least 1 vertexe each
      assert(a.size > 0)

      val summedRecord = Array.fill[Double](a.last.length)(0)
      a.foreach((record) => {
        for(i <- summedRecord.indices) {
          summedRecord(i) += record(i)
        }
      })
      val globalCentroid = summedRecord.map((v) => v / summedRecord.length)

      val distances = a.map(metric.distance(_, globalCentroid))
      val meanDistance = distances.sum / distances.size

      return (globalCentroid, meanDistance)
    })






  }
}

case class KMeansClassifier(clusters: Array[Cluster]) extends Classifier {

  def contains(x: RecordData)(implicit outth: OUTTH): Boolean = {
    clusters.exists(_.contains(x)(outth))
  }

  override def classify(x: RecordData): Label = {

    for(cluster_i <- clusters) {

      if(cluster_i.contains(x))
        return cluster_i.label

    }

    -1.0
  }
}

class Ensemble(maxClassifiers: Int = 10) extends Classifier {

  val classifiers = new ArrayBuffer[KMeansClassifier]()


  def addClassifier(x: KMeansClassifier): Unit = {

    // delete earliest classifier.
    if(classifiers.length + 1 > maxClassifiers)
      classifiers.remove(0)

    classifiers.append(x)
  }

  def contains(x: RecordData)(implicit outth: OUTTH): Boolean = {
    classifiers.exists(_.contains(x)(outth))
  }

  override def classify(x: RecordData): Label = {
    val map = new mutable.HashMap[Label, Int]()

    for(classifier_i <- classifiers) {
      val label = classifier_i.classify(x)
      map.update(label, map.getOrElse(label, 0) + 1)
    }

    // gets the (label,count) pair with max count, then returns the label
    map.maxBy((a) => a._2)._1
  }

  def weight(x: RecordData): Double = {
    throw NotImplementedException
  }


}



class Centroid(initial: Record) {

  var mean = initial.data

  val records = new ArrayBuffer[Record]()

  def addRecord(record:Record): Unit = {
    records += record
  }

  def clearRecords(): Unit = {
    records.clear()
  }

  def calculateMean(): RecordData = {

    assert(records.nonEmpty)

    val dimensions = records(0).data.length
    val output = Array.fill[Double](dimensions)(0)

    records.foreach((a) => {
      for(i <- a.data.indices)
        output(i) += a.data(i)
    })

    mean = output.map(_ / records.length)
    mean
  }

}




object Clusterers {

  def assignRecordToCentroid(record: Record, centroids: Array[Centroid], metric:DistanceMetric): Int = {
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

  def KMeans(records: Array[Record], k: Int, maxIterations: Int = 1000)(implicit distanceMetric: DistanceMetric = EuclideanDistance): Seq[Cluster] = {

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

    return centroids.map((a) => {
      var maxDist = 0.0
      val map = new mutable.HashMap[Label, Int]()

      a.records.foreach((r) => {
        maxDist = Math.max(maxDist, distanceMetric.distance(a.mean, r.data))
        map.put(r.label, map.getOrElse(r.label, 0) + 1)
      })

      Cluster(maxDist, a.mean, map.maxBy((a) => a._2)._1)
    })


  }


}
