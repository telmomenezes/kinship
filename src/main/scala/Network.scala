package com.telmomenezes.kinship

import com.telmomenezes.Aux._
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

class Network (val people: Map[Int, Person]) {
  val totalPeople = people.size
  
  val menCount = people.values.count(p => p.sex == "H")
  val womenCount = people.values.count(p => p.sex == "F")
  
  val edges = ((for (p <- people.values if p.fatherId > 0) yield (p.fatherId, p.id))
    ++ (for (p <- people.values if p.motherId > 0) yield (p.motherId, p.id))).toArray

  val rand = new Random()

  updateChildren()
  updateSiblings()
  updateDescendents()

  //println("cc: " + consistencyCheck)

  def this(peopleList: List[Array[String]]) = this((for (item <- peopleList) yield (item(0).toInt, new Person(item))).toMap)

  def this(net: Network) = this((for (p <- net.people) yield (p._1, new Person(p._2))).toMap)

  override def equals(other: Any) = {
    other match {
      case that: Network => this.people == that.people
      case _ => false
    }
  }

  def distance(that: Network) = (for (p <- people.values) yield p.distance(that.people(p.id))).reduceLeft(_ + _)

  def updateChildren() = {
    for (p <- people.values) {
      if (p.fatherId > 0) people(p.fatherId).addChild(p.id)
      if (p.motherId > 0) people(p.motherId).addChild(p.id)
    }
  }

  def clearChildren() = for (p <- people.values) p.clearChildren()

  def updateDescendents(origId: Int, targ: Person, visited: List[Int]): Unit = {
    if (!(visited contains targ.id)) {
      val targFatherId = targ.fatherId
      val targMotherId = targ.motherId
    
      if (targFatherId > 0) {
        val father = people(targFatherId)
        father.addDescendent(origId)
        updateDescendents(origId, father, targ.id :: visited)
      }
      if (targMotherId > 0) {
        val mother = people(targMotherId)
        mother.addDescendent(origId)
        updateDescendents(origId, mother, targ.id :: visited)
      }
    }
  }

  def updateDescendents(): Unit = {
    for ((key, p) <- people)
      updateDescendents(p.id, p, Nil)
  }

  def clearDescendents() = for (p <- people.values) p.clearDescendents()

  def updateSiblings() = {
    for (p <- people.values) {
      val children = p.children.toArray
      val n = children.size

      for (i <- 0 until n) {
        for (j <- (i + 1) until n) {
          val pi = people(children(i))
          val pj = people(children(j))
          // only consider full siblings
          if ((pi.fatherId == pj.fatherId) && (pi.motherId == pj.motherId)) {
            pi.addSibling(pj.id)
            pj.addSibling(pi.id)
          }
        }
      }
    }
  }

  def clearSiblings() = for (p <- people.values) p.clearSiblings()
  
  def sibCounts: MMap[Int, Int] = {
    val map = MMap[Int, Int]()
    for (p <- people.values) {
      val sibCount = p.siblings.size
      if (map contains sibCount) map(sibCount) += 1
      else map(sibCount) = 1 
    }
    map
  }

  def avgDescendents: Double = {
    var avg = 0.0
    for ((key, p) <- people) avg += p.totalDesc
    avg /= totalPeople
    avg
  }
  
  def randomEdgeList(k: Int): List[(Int, Int)] = {
    val indexes = MSet[Int]()
    while (indexes.size < k) indexes += rand.nextInt(edges.size)
    (for (i <- indexes) yield edges(i)).toList
  }

  def swappedEdgeList(edgeList: List[(Int, Int)]) = {
    val k = edgeList.size
    val shuffleTargs = rand.shuffle((0 until k).toList)
    (for (i <- 0 until k) yield (edgeList(i)._1, edgeList(shuffleTargs(i))._2)).toList
  }

  def edgeListDemog(edgeList: List[(Int, Int)]) = {
    val hh = edgeList.count(e => (people(e._1).sex == "H") && (people(e._2).sex == "H"))
    val hf = edgeList.count(e => (people(e._1).sex == "H") && (people(e._2).sex == "F"))
    val fh = edgeList.count(e => (people(e._1).sex == "F") && (people(e._2).sex == "H"))
    val ff = edgeList.count(e => (people(e._1).sex == "F") && (people(e._2).sex == "F"))
    (hh, hf, fh, ff)
  }

  def markSwaps(oldEdges: List[(Int, Int)], newEdges: List[(Int, Int)]) = {
    // update edge array
    for (i <- 0 until oldEdges.size) {
      if (oldEdges(i)._2 != newEdges(i)._2) {
        people(oldEdges(i)._1).swap = true
      }
    }
  }

  def removeEdges(oldEdges: List[(Int, Int)]) = {
    for (e <- oldEdges) {
      val childId = e._2
      val parentId = e._1
      if (people(childId).fatherId == parentId) people(childId).fatherId = 0
      else if (people(childId).motherId == parentId) people(childId).motherId = 0
      else {
        println(people(parentId))
        println(people(childId))
        println(e)
        assert(false)
      }
    }
  }

  def addEdges(newEdges: List[(Int, Int)]) = {
    for (e <- newEdges) {
      val childId = e._2
      val parentId = e._1
      if (people(parentId).sex == "H") people(childId).fatherId = parentId
      else if (people(parentId).sex == "F") people(childId).motherId = parentId
      else assert(false)
    }
  }

  def parentOverlap(newEdges: List[(Int, Int)]): Boolean = {
    for (e <- newEdges) {
      val parentId = e._1
      val childId = e._2
      if (people(parentId).sex == "H")
        if (people(childId).fatherId > 0) return true
      else if (people(parentId).sex == "F")
        if (people(childId).motherId > 0) return true
    }

    false
  }

  def checkLoops: Boolean = {
    for (p <- people.values)
      if (!p.checkLoops) return false
    return true
  }

  def randomSwap(k: Int): (Network, Boolean) = {
    val net = new Network(this)

    val oldEdges = net.randomEdgeList(k)
    val newEdges = net.swappedEdgeList(oldEdges)

    //println(oldEdges)
    //println(newEdges)

    // maintain demographic constraints
    val oldDemog = net.edgeListDemog(oldEdges)
    val newDemog = net.edgeListDemog(newEdges)
    if (oldDemog != newDemog) {
      //println("! demographic")
      return (this, false)
    }

    val oldSibCounts = sibCounts

    //println(oldEdges)
    //println(newEdges)
    net.removeEdges(oldEdges)

    // parent overlap
    if (net.parentOverlap(newEdges)) {
      //println("! parent overlap")
      return (this, false)
    }

    net.addEdges(newEdges)

    net.clearDescendents()
    net.updateDescendents()

    // check loops
    if (!net.checkLoops) {
      //println("! loops")
      return (this, false)
    }

    net.clearChildren()
    net.updateChildren()
    net.clearSiblings()
    net.updateSiblings()
    val newSibCounts = net.sibCounts

    //println(oldSibCounts)
    //println(newSibCounts)
    //if (oldSibCounts != newSibCounts) return (initNet, false)

    //replaceEdges(oldEdges, newEdges)
    net.markSwaps(oldEdges, newEdges)

    (net, true)
  }

  def swap(k: Int): (Network, Int) = {
    var count = 0
    var net = this
    while (true) {
      count += 1
      val res = net.randomSwap(k)
      net = res._1
      if (res._2) return (net, count)
    }

    assert(false)
    (this, -1)
  }

  def shuffle(k: Int): Network = {
    var origNet = new Network(this)
    var net = this
    val n = edges.size / k
    for (i <- 1 to n) {
      val res = net.swap(k)
      net = res._1
      val dist = net.distance(origNet)
      val relSwaps = (net.totalSwaps.toDouble / net.edges.size.toDouble) * 100
      val relDist = (dist.toDouble / net.edges.size.toDouble) * 100
      println("swap #" + i + " [" + res._2 + "]; total swaps: " + net.totalSwaps
        + "(" + relSwaps + "%); distance: " + dist + "(" + relDist + "%)")
    }
    net
  }

  def save(filePath: String) = {
    import java.io._
    val data = ("Id\tNom\tSexe\tpére\tmére\tconjoint") :: (for (p <- people.values) yield p.toFileLine).toList
    printToFile(new File(filePath))(p => {
      data.foreach(p.println)
    })
  }

  override def toString: String = {
    "total people:" + totalPeople + "; men: " + menCount + "; women: " + womenCount + "\n"
  }

  def printDiffs(that: Network) = {
    println("=====> DIFFS")
    for (pid <- people.keys)
      if (people(pid) != that.people(pid)) {
         println(that.people(pid))
         println(people(pid))
         println("-") 
      }
  }

  def consistencyCheck: Boolean = {
    for (p <- people.values) {
      if (p.fatherId > 0)
        if (people(p.fatherId).sex != "H") return false
      if (p.motherId > 0)
        if (people(p.motherId).sex != "F") return false
    }

    for (e <- edges) {
      val childId = e._2
      val parentId = e._1
      if ((people(childId).fatherId != parentId) && (people(childId).motherId != parentId)) 
        return false
    }

    return true
  }

  def totalSwaps: Int = people.count(p => p._2.swap)
}

object Network {
  def apply(filePath: String): Network = {
    val s = Source.fromFile(filePath)
    val people = (for (line <- s.getLines) yield line.split("\t")).toList.drop(1)
    new Network(people)
  }

  def main(args: Array[String]) {
    val n = Network("Chimane6.txt")
    println(n)
    println("avg descendents: " + n.avgDescendents)
    println("edge count: " + n.edges.size)

    println("sibling counts: " + n.sibCounts)

    println("consistent: " + n.consistencyCheck)

    n.shuffle(4)

    println(n)
    println("avg descendents: " + n.avgDescendents)
    println("edge count: " + n.edges.size)

    n.save("test.txt")
  }
}