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

    def getLabels(): mutable.Map[Label, Int] = {
      val set = new mutable.HashMap[Label, Int]()

      // this defines the meaninig of "Novel Class" - whether it is a label previously passed thorugh a classifier, or a label that is recognised by previous classifiers
//      clusters.foreach((c) => set += c.label)
//      records.foreach((r) => set.put(r.label, set.getOrElse(r.label, 0) + 1))
      clusters.foreach({case Cluster(r, c, l) => set.put(l, set.getOrElse(l, 0) + 1)})

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

    val classifiers = new ArrayBuffer[KMeansClassifier]()
    private val defaultReplacementPolicy = (ensemble: Ensemble, newClsf: KMeansClassifier) => {
      // This policy replaces earlierst classifier with the new one
      if(ensemble.classifiers.length + 1 > totalClassifiers)
        ensemble.classifiers.remove(0)

      ensemble.classifiers.append(newClsf)
    }

    def maxClassifiers = totalClassifiers

    def addClassifier(x: KMeansClassifier, replacementPolicy: (Ensemble, KMeansClassifier) => Unit = defaultReplacementPolicy): Unit = {
      defaultReplacementPolicy(this, x)
    }

    def getLabels(): mutable.Map[Label, Int] = {
      val set = new mutable.HashMap[Label, Int]()

      classifiers.foreach((c) => {
        c.getLabels().foreach((l) => set.put(l._1, set.getOrElse(l._1, 0) + l._2))
      })

      set
    }

    def contains(x: RecordData): Boolean = {
      classifiers.par.exists(_.contains(x))
    }

    override def classify(x: RecordData): Label = {
      // concurrent hashmap for good multithread performance
      val map = new java.util.concurrent.ConcurrentHashMap[Label, Int]()

      for(classifier_i <- classifiers.par) {
        val label = classifier_i.classify(x)

        map.put(label, map.getOrDefault(label, 0) + 1)
      }

      map.maxBy(_._2)._1
    }

    def weight(x: RecordData): Double = {
      val weights = classifiers.par.map((c) => c.weight(x))
      weights.max
    }
  }




}
