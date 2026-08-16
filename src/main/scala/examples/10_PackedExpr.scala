package examples

enum PackedExpr:
  case Pack[A](expression: Expr[A], render: A => String)

def runPacked(packed: PackedExpr): String =
  packed match
    case PackedExpr.Pack(expression, render) =>
      render(evaluate(expression))

@main def packedExprDemo(): Unit =
  val jobs: List[PackedExpr] =
    List(
      PackedExpr.Pack(
        Expr.Add(Expr.IntValue(10), Expr.IntValue(20)),
        value => s"integer result = $value"
      ),
      PackedExpr.Pack(
        Expr.LessThan(Expr.IntValue(3), Expr.IntValue(9)),
        value => s"Boolean result = $value"
      ),
      PackedExpr.Pack(
        Expr.TextValue("Scala"),
        value => s"text result = $value"
      )
    )

  jobs.map(runPacked).foreach(println)
