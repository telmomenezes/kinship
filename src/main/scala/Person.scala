package com.telmomenezes.kinship


import scala.collection.mutable.Set


class Person(data: Array[String]) {
	val id = data(0).toInt
	val name = data(1)
	val sex = data(2)

	var fatherId = data(3).toInt 
	var motherId = data(4).toInt

	var descendents = Set[Int]()
	
	def addDescendent(descId: Int) = descendents += descId
	
	def isDescendent(descId: Int): Boolean = descendents.contains(descId)

	def clearDescendents() = descendents = Set[Int]()

	def totalDesc = descendents.size
	
	def checkLoops: Boolean = (!(descendents contains fatherId)) && (!(descendents contains motherId))

	override def toString: String  = {
		"name: " + name + "; sex: " + sex + "; father: " + fatherId + "; mother: " + motherId
	}
}