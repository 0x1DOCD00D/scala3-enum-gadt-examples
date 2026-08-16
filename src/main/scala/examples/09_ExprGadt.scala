package examples

enum Expr[A]:
  case IntValue(value: Int) extends Expr[Int]
  case BoolValue(value: Boolean) extends Expr[Boolean]
  case TextValue(value: String) extends Expr[String]

  case Add(left: Expr[Int], right: Expr[Int]) extends Expr[Int]
  case Multiply(left: Expr[Int], right: Expr[Int]) extends Expr[Int]
  case LessThan(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean]
  case Concat(left: Expr[String], right: Expr[String]) extends Expr[String]

  case IfThenElse[A](
      condition: Expr[Boolean],
      whenTrue: Expr[A],
      whenFalse: Expr[A]
  ) extends Expr[A]

  case Pair[A, B](left: Expr[A], right: Expr[B]) extends Expr[(A, B)]
  case First[A, B](pair: Expr[(A, B)]) extends Expr[A]

def evaluate[A](expression: Expr[A]): A =
  expression match
    case Expr.IntValue(value) => value
    case Expr.BoolValue(value) => value
    case Expr.TextValue(value) => value
    case Expr.Add(left, right) => evaluate(left) + evaluate(right)
    case Expr.Multiply(left, right) => evaluate(left) * evaluate(right)
    case Expr.LessThan(left, right) => evaluate(left) < evaluate(right)
    case Expr.Concat(left, right) => evaluate(left) + evaluate(right)
    case Expr.IfThenElse(condition, whenTrue, whenFalse) =>
      if evaluate(condition) then evaluate(whenTrue) else evaluate(whenFalse)
    case Expr.Pair(left, right) => (evaluate(left), evaluate(right))
    case Expr.First(pair) => evaluate(pair)._1

def pretty[A](expression: Expr[A]): String =
  expression match
    case Expr.IntValue(value) => value.toString
    case Expr.BoolValue(value) => value.toString
    case Expr.TextValue(value) => "\"" + value + "\""
    case Expr.Add(left, right) => s"(${pretty(left)} + ${pretty(right)})"
    case Expr.Multiply(left, right) => s"(${pretty(left)} * ${pretty(right)})"
    case Expr.LessThan(left, right) => s"(${pretty(left)} < ${pretty(right)})"
    case Expr.Concat(left, right) => s"concat(${pretty(left)}, ${pretty(right)})"
    case Expr.IfThenElse(condition, whenTrue, whenFalse) =>
      s"if ${pretty(condition)} then ${pretty(whenTrue)} else ${pretty(whenFalse)}"
    case Expr.Pair(left, right) => s"(${pretty(left)}, ${pretty(right)})"
    case Expr.First(pair) => s"first(${pretty(pair)})"

def simplify[A](expression: Expr[A]): Expr[A] =
  expression match
    case Expr.Add(Expr.IntValue(0), right) => simplify(right)
    case Expr.Add(left, Expr.IntValue(0)) => simplify(left)
    case Expr.Add(Expr.IntValue(left), Expr.IntValue(right)) => Expr.IntValue(left + right)

    case Expr.Multiply(Expr.IntValue(1), right) => simplify(right)
    case Expr.Multiply(left, Expr.IntValue(1)) => simplify(left)
    case Expr.Multiply(Expr.IntValue(left), Expr.IntValue(right)) => Expr.IntValue(left * right)

    case Expr.Add(left, right) => Expr.Add(simplify(left), simplify(right))
    case Expr.Multiply(left, right) => Expr.Multiply(simplify(left), simplify(right))
    case Expr.LessThan(left, right) => Expr.LessThan(simplify(left), simplify(right))
    case Expr.Concat(left, right) => Expr.Concat(simplify(left), simplify(right))

    case Expr.IfThenElse(condition, whenTrue, whenFalse) =>
      simplify(condition) match
        case Expr.BoolValue(true) => simplify(whenTrue)
        case Expr.BoolValue(false) => simplify(whenFalse)
        case simplifiedCondition =>
          Expr.IfThenElse(
            simplifiedCondition,
            simplify(whenTrue),
            simplify(whenFalse)
          )

    case Expr.Pair(left, right) => Expr.Pair(simplify(left), simplify(right))
    case Expr.First(pair) => Expr.First(simplify(pair))
    case literal => literal

@main def expressionGadtDemo(): Unit =
  val arithmetic: Expr[Int] =
    Expr.Add(
      Expr.IntValue(3),
      Expr.Multiply(Expr.IntValue(4), Expr.IntValue(2))
    )

  println(pretty(arithmetic))
  println(evaluate(arithmetic))
  println(pretty(simplify(arithmetic)))

  val program: Expr[String] =
    Expr.IfThenElse(
      Expr.LessThan(
        Expr.Add(Expr.IntValue(2), Expr.IntValue(3)),
        Expr.IntValue(10)
      ),
      Expr.Concat(Expr.TextValue("result: "), Expr.TextValue("small")),
      Expr.TextValue("result: large")
    )

  println(pretty(program))
  println(evaluate(program))

  // Does not compile.
  // val invalid = Expr.Add(Expr.BoolValue(true), Expr.IntValue(1))
