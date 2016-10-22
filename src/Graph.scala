import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Graph[VertexType](initials: mutable.Set[VertexType]) {

  val vertices = initials
  val edges = new mutable.HashMap[VertexType, ArrayBuffer[VertexType]]()

  def addEdge(a: VertexType, b: VertexType): Unit = {

    assert(vertices.contains(a) && vertices.contains(b), "edge has vertices that dont exist in the graph!")

    val vertex = edges.getOrElse(a, new ArrayBuffer[VertexType]())
    vertex.append(b)
    edges.put(a, vertex)
  }

  def findConnectedComponents(): mutable.Set[mutable.Set[VertexType]] = {

    var components: mutable.Set[mutable.Set[VertexType]] = for(i <- vertices) yield new mutable.HashSet[VertexType]() += i

    // make sure each starting vertex of component is actual vertex
    components.foreach((c) => assert(vertices.contains(c.head), "components has bad vertices!!!???"))


    edges.foreach((edge) => {
      val (fromItem, toItem) = edge

      // find the set the fromItem belongs to
      val opt = components.find((v) => v.contains(fromItem))

      assert(opt.isDefined, "cannot find the set belonging to the starting vertex of the edge!!")

      val fromSet = opt.get

      // find the sets corresponding to each toItem
      val toSets = toItem.map((v) => components.find((a) => a.contains(v)).get)

      // add all members of each toSet into the fromSet
      toSets.foreach((set) => fromSet ++= set)

      components = components.filterNot((a) => a != fromSet && toSets.contains(a))
    })

    if(components.toArray.map(_.size).sum != vertices.size)
      assert(false, "some vertices were lost or gained during the process....")

    components
  }

}