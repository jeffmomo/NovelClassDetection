import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Graph[VertexType](initials: mutable.Set[VertexType]) {

  val vertices = initials
  val edges = new mutable.HashMap[VertexType, ArrayBuffer[VertexType]]()

  def addEdge(a: VertexType, b: VertexType): Unit = {

    assert(vertices.contains(a) && vertices.contains(b))

    val vertex = edges.getOrElse(a, new ArrayBuffer[VertexType]())
    vertex.append(b)
    edges.put(a, vertex)
  }

  def findConnectedComponents(): mutable.Set[mutable.Set[VertexType]] = {

    val components: mutable.Set[mutable.Set[VertexType]] = for(i <- vertices) yield new mutable.HashSet[VertexType]() += i

    // make sure each starting vertex of component is actual vertex
    components.foreach((c) => assert(vertices.contains(c.head)))


    edges.foreach((edge) => {
      val (fromItem, toItem) = edge

      // find the set the fromItem belongs to
      val opt = components.find((v) => v.contains(fromItem))
      if(opt.isEmpty)
        println("wtf")
      val fromSet = opt.get

      // find the sets corresponding to each toItem
      val toSets = toItem.map((v) => components.find((a) => a.contains(v)).get)

      // add all members of each toSet into the fromSet
      toSets.foreach((set) => fromSet ++= set)

      // remove the toSet from the components map, if we are not linking to self
      toSets.foreach((set) => if(set != fromSet) components -= set)
    })

    components
  }

}