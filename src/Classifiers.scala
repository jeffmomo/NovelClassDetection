import java.util.function.BiConsumer

import Helpers._
import RecordReader.Record

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions._

/**
  * Created by jeffmo on 17/10/16.
  */
object Classifiers {

  trait Classifier {
    def classify(x: RecordData): Label
  }



  case class KMeansClassifier(clusters: Array[Cluster], records: Array[Record])(implicit outth: OUTTH) extends Classifier {

    def contains(x: RecordData): Boolean = {
      clusters.exists(_.contains(x))
    }

    def weight(x: RecordData): Double = {
      val weights = clusters.map((c) => c.weight(x))
      weights.max
    }

    def getLabels(): mutable.Set[Label] = {
      val set = new mutable.HashSet[Label]()
      clusters.foreach((c) => set += c.label)

      set
    }

    override def classify(x: RecordData): Label = {

      clusters.foreach((cluster_i) => {
        if(cluster_i.contains(x))
          return cluster_i.label

      })

      -1.0
    }
  }


  class Ensemble(totalClassifiers: Int = 6)(implicit outth: OUTTH) extends Classifier {

    def maxClassifiers = totalClassifiers

    val classifiers = new ArrayBuffer[KMeansClassifier]()


    def addClassifier(x: KMeansClassifier): Unit = {


      // delete earliest classifier.
      if(classifiers.length + 1 > totalClassifiers)
        classifiers.remove(0)

      classifiers.append(x)
    }

    def getLabels(): mutable.Set[Label] = {
      val set = new mutable.HashSet[Label]()

      classifiers.foreach((c) => set ++= c.getLabels())

      set
    }

    def contains(x: RecordData): Boolean = {
      classifiers.par.exists(_.contains(x))
    }

    override def classify(x: RecordData): Label = {
      val map = new java.util.concurrent.ConcurrentHashMap[Label, Int]()

      for(classifier_i <- classifiers.par) {
        val label = classifier_i.classify(x)

        map.put(label, map.getOrDefault(label, 0) + 1)
      }

      // gets the (label,count) pair with max count (majority vote), then returns the label
//      var maxCount = 0
//      var label = 0.0
//      for(item: java.util.Map.Entry[Label, Int] <- map.entrySet()) {
//        if(item.getValue > maxCount) {
//          maxCount = item.getValue
//          label = item.getKey
//        }
//      }
      map.maxBy(_._2)._1
    }

    def weight(x: RecordData): Double = {
      val weights = classifiers.par.map((c) => c.weight(x))
      weights.max
//      throw new NotImplementedError()
//      -1
    }
  }




}
