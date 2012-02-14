package com.telmomenezes.kinship


import scala.collection.mutable.Set


class Person(val id: Int, val name: String, val sex: String, var fatherId: Int, var motherId: Int, var swap: Boolean = false) {
  var children = Set[Int]()
  var descendents = Set[Int]()
  var siblings = Set[Int]()

  def this(p: Person) = this(p.id, p.name, p.sex, p.fatherId, p.motherId, p.swap)
  def this(data: Array[String]) = this(data(0).toInt, data(1), data(2), data(3).toInt, data(4).toInt)

  override def equals(other: Any) = {
    other match {
      case that: Person => this.id == that.id && this.sex == that.sex &&
        this.fatherId == that.fatherId && this.motherId == that.motherId
      case _ => false
    }
  }

  def addChild(childId: Int) = children += childId
  def isChild(childId: Int): Boolean = children.contains(childId)
  def clearChildren() = children = Set[Int]()
  def totalChildren = children.size
  
  def addDescendent(descId: Int) = descendents += descId
  def isDescendent(descId: Int): Boolean = descendents.contains(descId)
  def clearDescendents() = descendents = Set[Int]()
  def totalDesc = descendents.size

  def addSibling(sibId: Int) = siblings += sibId
  def isSibling(sibId: Int): Boolean = siblings.contains(sibId)
  def clearSiblings() = siblings = Set[Int]()
  def totalSiblings = siblings.size
  
  def checkLoops: Boolean = (!(descendents contains fatherId)) && (!(descendents contains motherId))

  def toFileLine: String  = {
    "" + id + "\t" + name + "\t" + sex + "\t" + fatherId + "\t" + motherId
  }

  override def toString: String  = {
    "name: " + name + "; sex: " + sex + "; father: " + fatherId + "; mother: " + motherId
  }
}