package com.telmomenezes.kinship

import com.telmomenezes.Aux._
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.{Map => MMap}

class Network (peopleList: List[Array[String]]) {
  val people: Map[Int, Person] = (for (item <- peopleList) yield (item(0).toInt, new Person(item))).toMap
  val totalPeople = people.size
  
  val menCount = people.values.count(p => p.sex == "H")
  val womenCount = people.values.count(p => p.sex == "F")
  
  val edges = ((for (p <- people.values if p.fatherId > 0) yield (p.fatherId, p.id))
    ++ (for (p <- people.values if p.motherId > 0) yield (p.motherId, p.id))).toArray

  val rand = new Random()

  def updateChildren() = {
    for (p <- people.values) {
      if (p.fatherId > 0) people(p.fatherId).addChild(p.id)
      if (p.motherId > 0) people(p.motherId).addChild(p.id)
    }
  }

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

  def clearDescendents(): Unit = {
    for (p <- people.values)
      p.clearDescendents()
  }

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

  def clearSiblings() = {
    for (p <- people.values)
      p.clearSiblings()
  }
  
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
    for ((parentId, childId) <- oldEdges)
      if (people(childId).fatherId == parentId) people(childId).fatherId = 0
      else if (people(childId).motherId == parentId) people(childId).motherId = 0
  }

  def addEdges(newEdges: List[(Int, Int)]) = {
    for ((parentId, childId) <- newEdges)
      if (people(parentId).sex == "H") people(childId).fatherId = parentId
      else if (people(parentId).sex == "F") people(childId).motherId = parentId
  }

  def parentOverlap(newEdges: List[(Int, Int)]): Boolean = {
    for ((parentId, childId) <- newEdges)
      if (people(parentId).sex == "H")
        if (people(childId).fatherId > 0)
          return false
      else if (people(parentId).sex == "F")
        if (people(childId).motherId > 0)
          return false

    true
  }

  def checkLoops: Boolean = {
    for (p <- people.values)
      if (!p.checkLoops) return false
    return true
  }

  def randomSwap(k: Int): Boolean = {
    val oldEdges = randomEdgeList(k)
    val newEdges = swappedEdgeList(oldEdges)
    
    // maintain demographic constraints
    val oldDemog = edgeListDemog(oldEdges)
    val newDemog = edgeListDemog(newEdges)
    if (oldDemog != newDemog) {
      //println("FAIL: demographic constraints")
      return false
    }

    removeEdges(oldEdges)

    // parent overlap
    if (parentOverlap(newEdges)) {
      //println("FAIL: parent overlap")
      // restore initil state
      addEdges(oldEdges)
      return false
    }

    addEdges(newEdges)

    clearDescendents()
    updateDescendents()

    // check loops
    if (!checkLoops) {
      //println("FAIL: loops")
      // restore initil state
      removeEdges(newEdges)
      addEdges(oldEdges)
      clearDescendents()
      updateDescendents()
      return false
    }

    replaceEdges(oldEdges, newEdges)

    true
  }

  def swap(k: Int): Int = {
    var count = 0
    while (true) {
      count += 1
      if (randomSwap(k)) return count
    }

    -1
  }

  def shuffle(k: Int) = {
    val n = edges.size / k
    for (i <- 1 to n) {
      println("swap #" + i + " [" + swap(k) + "]")
    }
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
}

object Network {
  def net(filePath: String): Network = {
    val s = Source.fromFile(filePath)
    val people = (for (line <- s.getLines) yield line.split("\t")).toList.drop(1)
    val n = new Network(people)
    n.updateChildren()
    n.updateSiblings()
    n.updateDescendents()
    n
  }

  def main(args: Array[String]) {
    val n = Network.net("Chimane6.txt")
    println(n)
    println("avg descendents: " + n.avgDescendents)
    println("edge count: " + n.edges.size)

    println("sibling counts: " + n.sibCounts)
    /*
    n.shuffle(8)

    println(n)
    println("avg descendents: " + n.avgDescendents)
    println("edge count: " + n.edges.size)

    n.save("test.txt") */
  }
}