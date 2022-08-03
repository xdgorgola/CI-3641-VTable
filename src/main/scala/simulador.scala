/** Simulador principal */

import scala.collection.mutable
import objclases.ObjClases
import scala.io.StdIn.readLine

val TOKEN_DESCRIBIR = "DESCRIBIR"
val TOKEN_CLASS = "CLASS"
val TOKEN_SALIR = "SALIR"

var clases = mutable.Map[String, ObjClases]()

def simulator_usage() =
  println("Uso:\n\tCLASS <tipo> [<nombre>]")
  println("\t\tDefine un nuevo tipo que poseerá métodos con nombres establecidos en la lista proporcionada.")

  println("\n\tDESCRIBIR <NOMBRE>")
  println("\t\tMuestra la tabla de metodos virtuales del tipo <NOMBRE>")

  println("\n\tSALIR\n\t\tMata\\sale del programa.")


def wrong_params() =
  println("Parametro invalido o numero de parametros incorrecto.")
  simulator_usage()


/** Calcula si hay elementos repetidos en la cola de tokens
 *
 * @param tokens Cola de tokens a chequear
 * @return true si hay tokens repetidos. false en caso contrario
 */
def tokens_repetidos(tokens: mutable.Queue[String]): Boolean =
  for t <- tokens do
    if tokens.count(x => x == t) > 1 then
      return true
  false


def comando_class(tokens: mutable.Queue[String]): Unit =
  if tokens.length < 1 then
    wrong_params()
    return ()

  val nombre = tokens.dequeue()
  if clases.contains(nombre) then
    println(f"El nombre $nombre ya ha sido definido.")
    return ()

  if tokens.length > 0 then
    if tokens(0) == ":" then
      tokens.dequeue()
      if tokens.length == 0 then
        wrong_params()
        return ()

      if tokens_repetidos(tokens) then
        println(f"Hay definiciones de metodos repetidas.")
        wrong_params()
        return ()

      val padre = tokens.dequeue()
      if !clases.contains(padre) then
        println(f"La clase $padre no existe.")
        wrong_params()
        return ()

      clases(nombre) = ObjClases(nombre, clases(padre), tokens.toList)
      println(f"Clase $nombre creada exitosamente.")
      return ()

  if tokens_repetidos(tokens) then
    println(f"Hay definiciones de metodos repetidas.")
    wrong_params()
    return ()

  clases(nombre) = ObjClases(nombre, null, tokens.toList)
  println(f"Clase $nombre creada exitosamente.")


def comando_describir(tokens: mutable.Queue[String]): Unit =
  if tokens.length != 1 then
    wrong_params()
    return ()

  val clase = tokens.dequeue()

  if (!clases.contains(clase)) then
    println(s"La clase de nombre $clase no ha sido definida.")
    return ()

  clases(clase).describir()


@main def main() =
  var run = true
  while run do
    print("Da tu orden, creador>")
    var input = readLine()
    val tokens = mutable.Queue[String](input.split(' '): _*)
    input = tokens.dequeue().toUpperCase()

    input match
      case TOKEN_CLASS => comando_class(tokens)
      case TOKEN_DESCRIBIR => comando_describir(tokens)
      case TOKEN_SALIR => run = false
      case _ => wrong_params()