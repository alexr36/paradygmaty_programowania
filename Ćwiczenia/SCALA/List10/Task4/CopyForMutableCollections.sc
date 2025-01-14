//  ZADANIE 4

//  Implementation
def copy[T](dest: collection.mutable.Seq[T], src: collection.mutable.Seq[T]): Unit = {
  var i = 0

  src.foreach({
    x => dest.update(i, x)
    i += 1
  })
}

//  Tests
val src = collection.mutable.Seq(1, 2, 3)
val dest = collection.mutable.Seq(0, 0, 0)

println(s"Before copy: src = $src, dest = $dest.")
copy(dest, src)
println(s"After copy: src = $src, dest = $dest.")
