import Classifiers.Ensemble
import Helpers.{OUTTH, RecordData}
import RecordReader.Record

/**
  * Created by jeffmo on 16/10/16.
  */



//Elevation / quantitative /meters / Elevation in meters
//Aspect / quantitative / azimuth / Aspect in degrees azimuth
//  Slope / quantitative / degrees / Slope in degrees
//Horizontal_Distance_To_Hydrology / quantitative / meters / Horz Dist to nearest surface water features
//Vertical_Distance_To_Hydrology / quantitative / meters / Vert Dist to nearest surface water features
//Horizontal_Distance_To_Roadways / quantitative / meters / Horz Dist to nearest roadway
//Hillshade_9am / quantitative / 0 to 255 index / Hillshade index at 9am, summer solstice
//  Hillshade_Noon / quantitative / 0 to 255 index / Hillshade index at noon, summer soltice
//  Hillshade_3pm / quantitative / 0 to 255 index / Hillshade index at 3pm, summer solstice
//  Horizontal_Distance_To_Fire_Points / quantitative / meters / Horz Dist to nearest wildfire ignition points
//Wilderness_Area (4 binary columns) / qualitative / 0 (absence) or 1 (presence) / Wilderness area designation
//Soil_Type (40 binary columns) / qualitative / 0 (absence) or 1 (presence) / Soil Type designation
//Cover_Type (7 types) / integer / 1 to 7 / Forest Cover Type designation
//


object Main {
  def main(args: Array[String]): Unit = {

    val records = RecordReader.getRecords("covtype.data")

    assert(records.length > 0)

    normalise_mutable(records)



//    val reordered = records.sortBy((a) => a.data(0))
//    val reordered = new DepletingRandomiser(records.length).map((idx) => records(idx)).toArray
    val reordered = records

    implicit val outth = new OUTTH(0.7, 0.01, 10, false)
    Evaluator.evaluateEnsemble(reordered, new Ensemble(6), 1000)






  }


  def normalise_mutable(records: Array[Record]): Unit = {
    val dimensions = records(0).data.length

    val (mins, maxes) = records.foldRight((new RecordData(dimensions), new RecordData(dimensions)))((a, accum) => {
      val (mins, maxes) = accum
      for (i <- a.data.indices) {
        mins(i) = Math.min(mins(i), a.data(i))
        maxes(i) = Math.max(maxes(i), a.data(i))
      }
      (mins, maxes)
    })
    val ranges = for (i <- mins.indices) yield maxes(i) - mins(i)

    // normalise records
    records.par.foreach({
      case Record(d, l) => {
        for (i <- d.indices if ranges(i) != 0) {
          d(i) = (d(i) - mins(i)) / ranges(i)
          assert(d(i) <= 1 && d(i) >= 0)
        }
      }
    })
  }
}
