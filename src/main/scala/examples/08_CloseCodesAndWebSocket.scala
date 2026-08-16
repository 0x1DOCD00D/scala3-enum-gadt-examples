package examples

sealed trait CloseCode:
  def value: Int

enum StandardCloseCode(val value: Int) extends CloseCode:
  case NormalClosure extends StandardCloseCode(1000)
  case GoingAway extends StandardCloseCode(1001)
  case ProtocolError extends StandardCloseCode(1002)
  case UnsupportedData extends StandardCloseCode(1003)

object CloseCode:
  private final case class Custom(value: Int) extends CloseCode

  def fromInt(value: Int): Either[String, CloseCode] =
    StandardCloseCode.values.find(_.value == value) match
      case Some(standard) => Right(standard)
      case None if value >= 3000 && value <= 4999 => Right(Custom(value))
      case None => Left(s"Invalid WebSocket close code $value")

sealed trait WebSocketState
sealed trait Open extends WebSocketState
sealed trait Closing extends WebSocketState
sealed trait Closed extends WebSocketState

enum WebSocketCommand[
    From <: WebSocketState,
    To <: WebSocketState,
    A
]:
  case Send(text: String) extends WebSocketCommand[Open, Open, Unit]
  case BeginClose(code: CloseCode) extends WebSocketCommand[Open, Closing, Unit]
  case FinishClose extends WebSocketCommand[Closing, Closed, Unit]

final class Session[S <: WebSocketState] private (val id: String):
  override def toString: String = s"Session($id)"

object Session:
  def open(id: String): Session[Open] =
    new Session[Open](id)

  def run[
      From <: WebSocketState,
      To <: WebSocketState,
      A
  ](
      session: Session[From],
      command: WebSocketCommand[From, To, A]
  ): (Session[To], A) =
    command match
      case WebSocketCommand.Send(text) =>
        println(s"${session.id}: sending '$text'")
        (new Session[Open](session.id), ())

      case WebSocketCommand.BeginClose(code) =>
        println(s"${session.id}: beginning close with ${code.value}")
        (new Session[Closing](session.id), ())

      case WebSocketCommand.FinishClose =>
        println(s"${session.id}: closed")
        (new Session[Closed](session.id), ())

@main def webSocketStateDemo(): Unit =
  val code = CloseCode.fromInt(1000).toOption.get
  val custom = CloseCode.fromInt(4001)
  val invalid = CloseCode.fromInt(2500)

  println(code)
  println(custom)
  println(invalid)

  val session0: Session[Open] = Session.open("socket-17")
  val (session1, _) = Session.run(session0, WebSocketCommand.Send("hello"))
  val (session2, _) = Session.run(session1, WebSocketCommand.BeginClose(code))
  val (session3, _) = Session.run(session2, WebSocketCommand.FinishClose)

  println(session3)

  // Does not compile because FinishClose requires Session[Closing].
  // Session.run(session0, WebSocketCommand.FinishClose)
