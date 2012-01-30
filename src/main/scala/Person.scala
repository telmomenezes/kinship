package kinship


import scala.collection.mutable.Set


class Person(val id: Int, val name: String, val sex: String, val fatherId: Int, val motherId: Int) {

	val descendents = Set[Int]()
	var totalDesc = 0
	
	def addDescendent(descId: Int) = {
		descendents += descId
		totalDesc += 1
	}
	
	def isDescendent(descId: Int): Boolean = {
		descendents.contains(descId)
	}
	
	override def toString: String  = {
		"name: " + name + "; sex: " + sex + "; father: " + fatherId + "; mother: " + motherId
	}
}