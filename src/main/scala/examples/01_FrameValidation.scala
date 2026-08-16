package examples

final case class FrameValidationError(
    frame: String,
    actualSize: Int,
    expected: String
)

enum FrameType(
    val wireCode: Int,
    val payloadValidator: Option[Int => Boolean],
    val expectedPayload: String
):
  case Data extends FrameType(0, None, "any size")
  case Headers extends FrameType(1, None, "any size")
  case Priority extends FrameType(2, Some(size => size == 5), "exactly 5 bytes")
  case Reset extends FrameType(3, Some(size => size == 4), "exactly 4 bytes")
  case Settings extends FrameType(4, Some(size => size % 6 == 0), "a multiple of 6 bytes")
  case PushPromise extends FrameType(5, Some(size => size >= 4), "at least 4 bytes")
  case Ping extends FrameType(6, Some(size => size == 8), "exactly 8 bytes")

  def validatePayload(size: Int): Either[FrameValidationError, Unit] =
    payloadValidator match
      case None => Right(())
      case Some(accepts) if accepts(size) => Right(())
      case Some(_) => Left(FrameValidationError(toString, size, expectedPayload))

enum PayloadRule:
  case AnySize
  case Exactly(bytes: Int)
  case MultipleOf(divisor: Int)
  case AtLeast(bytes: Int)

  def accepts(actual: Int): Boolean =
    this match
      case AnySize => true
      case Exactly(bytes) => actual == bytes
      case MultipleOf(divisor) => divisor > 0 && actual % divisor == 0
      case AtLeast(bytes) => actual >= bytes

  def description: String =
    this match
      case AnySize => "any size"
      case Exactly(bytes) => s"exactly $bytes bytes"
      case MultipleOf(divisor) => s"a multiple of $divisor bytes"
      case AtLeast(bytes) => s"at least $bytes bytes"

enum FrameSpec(val wireCode: Int, val payloadRule: PayloadRule):
  case Data extends FrameSpec(0, PayloadRule.AnySize)
  case Priority extends FrameSpec(2, PayloadRule.Exactly(5))
  case Reset extends FrameSpec(3, PayloadRule.Exactly(4))
  case Settings extends FrameSpec(4, PayloadRule.MultipleOf(6))
  case PushPromise extends FrameSpec(5, PayloadRule.AtLeast(4))
  case Ping extends FrameSpec(6, PayloadRule.Exactly(8))

  def validate(size: Int): Either[String, Unit] =
    if payloadRule.accepts(size) then Right(())
    else Left(s"$this requires ${payloadRule.description}, received $size bytes")

sealed trait FrameScope
sealed trait ConnectionLevel extends FrameScope
sealed trait StreamLevel extends FrameScope

opaque type StreamId = Int

object StreamId:
  def fromInt(value: Int): Either[String, StreamId] =
    if value > 0 then Right(value)
    else Left(s"A stream identifier must be positive, received $value")

  extension (id: StreamId)
    def value: Int = id

enum Target[S <: FrameScope]:
  case Connection extends Target[ConnectionLevel]
  case Stream(id: StreamId) extends Target[StreamLevel]

enum TypedFrameType[S <: FrameScope](
    val wireCode: Int,
    val payloadRule: PayloadRule
):
  case Data extends TypedFrameType[StreamLevel](0, PayloadRule.AnySize)
  case Headers extends TypedFrameType[StreamLevel](1, PayloadRule.AnySize)
  case Priority extends TypedFrameType[StreamLevel](2, PayloadRule.Exactly(5))
  case Settings extends TypedFrameType[ConnectionLevel](4, PayloadRule.MultipleOf(6))
  case Ping extends TypedFrameType[ConnectionLevel](6, PayloadRule.Exactly(8))

final case class Frame[S <: FrameScope](
    frameType: TypedFrameType[S],
    target: Target[S],
    payload: Vector[Byte]
)

@main def frameValidationDemo(): Unit =
  println(FrameType.Ping.validatePayload(7))
  println(FrameType.Ping.validatePayload(8))
  println(FrameSpec.Settings.validate(12))

  val streamId = StreamId.fromInt(7).toOption.get

  val dataFrame: Frame[StreamLevel] =
    Frame(TypedFrameType.Data, Target.Stream(streamId), Vector.empty)

  val pingFrame: Frame[ConnectionLevel] =
    Frame(TypedFrameType.Ping, Target.Connection, Vector.fill(8)(0.toByte))

  println(dataFrame)
  println(pingFrame)

  // Does not compile because Ping is connection-level while Target.Stream is stream-level.
  // val invalid = Frame(TypedFrameType.Ping, Target.Stream(streamId), Vector.fill(8)(0.toByte))
