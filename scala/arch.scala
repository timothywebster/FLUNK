val _s1 = "brillig and the wabe troves did gimble in gyre"
val _s2 = "twas and the slithy troves did gimble wabe"

val _s3 = "dotes dozy dotes iddle amzy divvy"
val _s4 = "marzy dotes dozy amzy divvy"


def compare(s1:String, s2:String) = {
	val l1 = s1.split("\\s")
	val l2 = s2.split("\\s")
	val n = l1.toList intersect l2.toList
	val r1 = l1.filter(n contains _).mkString(".*")
	val r2 = l2.filter(n contains _).mkString(".*")
	(n.length, s2.matches(r1) || s1.matches(r2))
}


