package examples

enum Equal[A, B]:
  case Refl[A]() extends Equal[A, A]

def substitute[A, B](evidence: Equal[A, B], value: A): B =
  evidence match
    case Equal.Refl() => value

@main def equalityProofDemo(): Unit =
  val proof: Equal[String, String] = Equal.Refl[String]()
  val result: String = substitute(proof, "type-safe substitution")
  println(result)
