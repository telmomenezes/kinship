package kinship


import scala.collection.mutable.Set


class Person(val id: Int, val name: String, val sex: String, val fatherId: Int, val motherId: Int) {

	val descendents = Set[Int]()
	
	def addDescendent(descId: Int) = descendents += descId
	
	def isDescendent(descId: Int): Boolean = descendents.contains(descId)

	def totalDesc = descendents.size
	
	override def toString: String  = {
		"name: " + name + "; sex: " + sex + "; father: " + fatherId + "; mother: " + motherId
	}
}