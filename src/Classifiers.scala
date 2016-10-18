import Helpers._
import RecordReader.Record

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
      classifiers.exists(_.contains(x))
    }

    override def classify(x: RecordData): Label = {
      val map = new mutable.HashMap[Label, Int]()

      for(classifier_i <- classifiers) {
        val label = classifier_i.classify(x)

        map.update(label, map.getOrElse(label, 0) + 1)
      }

      // gets the (label,count) pair with max count (majority vote), then returns the label
      map.maxBy((a) => a._2)._1
    }

    def weight(x: RecordData): Double = {
      val weights = classifiers.map((c) => c.weight(x))
      weights.max
//      throw new NotImplementedError()
//      -1
    }
  }




}
