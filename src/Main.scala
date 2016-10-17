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

    RecordReader.getRecords("memes.csv")



  }

}
