###Novel Class Detector for Data Streams
An implementation from the paper (http://www.cs.uiuc.edu/~hanj/pdf/icdm10_mmasud.pdf)



Install Instructions
----
Prerequisites: Scala 2.11.8

To build:
----
```
git clone https://github.com/jeffmomo/NovelClassDetection.git
cd NovelClassDetection
scalac src/*.scala -d out.jar
```

Usage:
----
We need to increase the VM memory limits for the forest cover dataset under standard parameters

```
scala -J-Xmx4g out.jar dataset=path [order=default|random|sorted_by_first_col] [normalize=true|false] [outth_clamp=false|true] [kmeans_iters=Integer] [ensemble_size=Integer] [chunk_size=Integer]

# example run
scala -J-Xmx4g out.jar dataset=covtype.data order=default normalize=true kmeans_iters=100 ensemble_size=6

```

The default dataset is the UCI Forest cover dataset 'covtype.data'