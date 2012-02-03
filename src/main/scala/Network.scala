package com.telmomenezes.kinship


class Network (peopleList: List[Array[String]]) {
	val people: Map[Int, Person] = (for (item <- peopleList) yield (item(0).toInt, new Person(item))).toMap
	val totalPeople = people.size
	val menCount = people.values.count(p => p.sex == "H")
	val womenCount = people.values.count(p => p.sex == "F")
	
	def updateDescendents(origId: Int, targ: Person): Unit = {
		val targFatherId = targ.fatherId
		val targMotherId = targ.motherId
		
		if (targFatherId > 0) {
			val father = people(targFatherId)
			father.addDescendent(origId)
			updateDescendents(origId, father)
		}
		if (targMotherId > 0) {
			val mother = people(targMotherId)
			mother.addDescendent(origId)
			updateDescendents(origId, mother)
		}
	}

	def updateDescendents(): Unit = {
		for ((key, p) <- people)
			updateDescendents(p.id, p)
	}
	
	def avgDescendents: Double = {
		var avg = 0.0
		for ((key, p) <- people) {
			avg += p.totalDesc
		}
		avg /= totalPeople
		avg
	}
	
	override def toString: String = {
		"total people:" + totalPeople + "; men: " + menCount + "; women: " + womenCount + "\n"
	}
}


import scala.io.Source

object Network {
	def net(filePath: String): Network = {
		val s = Source.fromFile(filePath)
  		val people = (for (line <- s.getLines) yield line.split("\t")).toList.drop(1)
		val n = new Network(people)
		n.updateDescendents()
		n
	}

	def main(args: Array[String]) {
		val n = Network.net("Chimane6.txt")
		println(n)
		println("avg descendents: " + n.avgDescendents)
	}
}