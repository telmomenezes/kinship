package com.telmomenezes.kinship

import com.telmomenezes.Aux._
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.{Map => MMap}

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

  def this(peopleList: List[Array[String]]) = this((for (item <- peopleList) yield (item(0).toInt, new Person(item))).toMap)

  def this(net: Network) = this((for (p <- net.people) yield (p._1, new Person(p._2))).toMap)

  override def equals(other: Any) = {
    other match {
      case that: Network => this.people == that.people
      case _ => false
    }
  }

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
  
  def randomEdgeList(k: Int): List[(Int, Int)] = (for (i <- 0 until k) yield edges(rand.nextInt(edges.size))).toList

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

  def replaceEdges(oldEdges: List[(Int, Int)], newEdges: List[(Int, Int)]) = {
    // update edge array
    for (i <- 0 until edges.size)
      for (j <- 0 until oldEdges.size)
        if (edges(i) == oldEdges(j)) edges(i) = newEdges(j)
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
    val initNet = new Network(this)

    val oldEdges = randomEdgeList(k)
    val newEdges = swappedEdgeList(oldEdges)

    //println(oldEdges)
    //println(newEdges)

    // maintain demographic constraints
    val oldDemog = edgeListDemog(oldEdges)
    val newDemog = edgeListDemog(newEdges)
    if (oldDemog != newDemog) return (initNet, false)

    val oldSibCounts = sibCounts

    removeEdges(oldEdges)

    // parent overlap
    if (parentOverlap(newEdges)) return (initNet, false)

    addEdges(newEdges)

    clearDescendents()
    updateDescendents()

    // check loops
    if (!checkLoops) return (initNet, false)

    clearChildren()
    updateChildren()
    clearSiblings()
    updateSiblings()
    val newSibCounts = sibCounts

    //println(oldSibCounts)
    //println(newSibCounts)
    if (oldSibCounts != newSibCounts) return (initNet, false)

    replaceEdges(oldEdges, newEdges)

    (this, true)
  }

  def swap(k: Int): (Network, Int) = {
    var count = 0
    var net = new Network(this)
    while (true) {
      count += 1
      val res = net.randomSwap(k)
      if (res._2) return (res._1, count)
      net = res._1
    }

    assert(false)
    (this, -1)
  }

  def shuffle(k: Int): Network = {
    var net = this
    val n = edges.size / k
    for (i <- 1 to n) {
      val res = net.swap(k)
      net = res._1
      println("swap #" + i + " [" + res._2 + "]")
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