package kinship


import scala.collection.mutable.Map
import scala.io.Source


class Network {
	val people = Map[Int, Person]()
	var totalPeople = 0
	var menCount = 0
	var womenCount = 0
	var mm = 0
	var mw = 0
	var wm = 0
	var ww = 0
	
	def addPerson(id: Int, name: String, sex: String, fatherId: Int, motherId: Int) = {
		val person = new Person(id, name, sex, fatherId, motherId)
		people(id) = person
		
		totalPeople += 1
		if (sex == "H")
			menCount += 1
		else if (sex == "F")
			womenCount += 1
	}
	
	def updateDemographicMetrics() = {
		for ((key, p) <- people) {
			if (p.fatherId > 0) {
				if (p.sex == "H")
					mm += 1
				else if (p.sex == "F")
					mw += 1
			}
			if (p.motherId > 0) {
				if (p.sex == "H")
					wm += 1
				else if (p.sex == "F")
					ww += 1
			}
		}
	}
	
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
		println(avg)
		avg /= totalPeople
		avg
	}
	
	def load(filePath: String) = {
		var firstLine = true
		
		val s = Source.fromFile(filePath)
  		s.getLines.foreach ( (line) => {
  			// skip header row
  			if (firstLine) {
  				firstLine = false
  			}
  			// parse rows
  			else {
				var id = 0
				var name = ""
				var sex = ""
				var fatherId = 0
				var motherId = 0
				
				val tokens = line.split("\t")
				var row = 0
				for (t <- tokens) {
					row match {
						case 0 => id = t.toInt
						case 1 => name = t
						case 2 => sex = t
						case 3 => fatherId = t.toInt
						case 4 => motherId = t.toInt
						case _ => false
					}
					row += 1
				}
				
				addPerson(id, name, sex, fatherId, motherId)
			}
		})
			
		updateDemographicMetrics()
		updateDescendents()
	}
	
	override def toString: String = {
		"total people:" + totalPeople + "; men: " + menCount + "; women: " + womenCount + "\n" +
		"mm: " + mm + "; mw: " + mw + "; wm: " + wm + "; ww: " + ww + "\n"
	}
}

object Network {
	def main(args: Array[String]) {
		val net = new Network
		net.load("Chimane6.txt")
		println(net)
		val avgDesc = net.avgDescendents 
		println("avg descendents: " + avgDesc)
	}
}