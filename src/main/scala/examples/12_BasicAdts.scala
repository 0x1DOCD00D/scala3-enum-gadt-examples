package examples

final case class Credentials(username: String, passwordHash: String)

enum Authentication:
  case Password(credentials: Credentials)
  case ApiKey(value: String)
  case Certificate(subject: String, fingerprint: String)

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

enum Request:
  case CopyFile(fileName: String, destination: String)
  case DeleteFile(fileName: String)
  case StopProcess(processId: Long)

enum TypedBox[A]:
  case IntBox(value: Int) extends TypedBox[Int]
  case BooleanBox(value: Boolean) extends TypedBox[Boolean]

def transform[A](box: TypedBox[A]): A =
  box match
    case TypedBox.IntBox(value) => value + 1
    case TypedBox.BooleanBox(value) => !value

@main def basicAdtsDemo(): Unit =
  val request = Request.CopyFile("a.txt", "/tmp/a.txt")
  val tree: Tree[Int] = Tree.Branch(Tree.Leaf(1), Tree.Leaf(2))
  println(request)
  println(tree)
  println(transform(TypedBox.IntBox(41)))
  println(transform(TypedBox.BooleanBox(false)))
