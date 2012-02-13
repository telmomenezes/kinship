package com.telmomenezes.kinship


import scala.collection.mutable.Set


class Person(data: Array[String]) {
	val id = data(0).toInt
	val name = data(1)
	val sex = data(2)

	var fatherId = data(3).toInt 
	var motherId = data(4).toInt

	var children = Set[Int]()
	var descendents = Set[Int]()
	var siblings = Set[Int]()

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