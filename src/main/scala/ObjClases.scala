import scala.collection.mutable.LinkedHashMap

class ObjClases(val nombre: String, val padre: ObjClases, metodos: List[String])
{
  var vTable = LinkedHashMap[String, String]()
  if padre != null then
    for (n, m) <- padre.vTable do vTable(n) = m

  for m <- metodos do vTable(m) = nombre

  def describir(): Unit =
    for (n, m) <- vTable do println(s"$n -> $m :: $n")
}

@main def main() =
  val claseA = ObjClases("A", null, List("f", "g", "h"))
  val claseB = ObjClases("B", claseA, List("f", "h", "a"))
  claseA.describir()
  println()
  claseB.describir()
