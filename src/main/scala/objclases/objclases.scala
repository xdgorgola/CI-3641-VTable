package objclases

import scala.collection.mutable.LinkedHashMap

/** Clase que representa una clase con vTable
 *
 * @param nombre Nombre de la clase
 * @param padre Padre de la casa. null si no tiene.
 * @param metodos Lista de metodos definidos en la clase
 */
class ObjClases(val nombre: String, val padre: ObjClases, metodos: List[String])
{
  var vTable = LinkedHashMap[String, String]()
  if padre != null then
    for (n, m) <- padre.vTable do vTable(n) = m

  for m <- metodos do vTable(m) = nombre

  /** Describe la vTable de una clase */
  def describir(): Unit =
    for (n, m) <- vTable do println(s"$n -> $m :: $n")
}
