package kinship


class Person(id: Int, name: String, sex: String, fatherId: Int, motherId: Int) {
	
	val descendents = new Set[Int]
	var totalDesc = 0
	
	def addDescendent(descId: Int) {
		descendents += descId)
		totalDesc += 1
	}
	
	def isDescendent(descId: Int): Boolean {
		descendents.contains(descId)
	}
	
	override def toString: String {
		"name: " + name + "; sex: " + sex + "; father: " + fatherId + "; mother: " + motherId
	}
}