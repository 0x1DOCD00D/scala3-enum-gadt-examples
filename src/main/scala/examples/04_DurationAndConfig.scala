package examples

import java.time.Duration
import scala.util.Try

enum DurationStyle:
  case Simple
  case Iso8601

  def parse(
      text: String,
      defaultUnit: DurationStyle.DurationUnit = DurationStyle.DurationUnit.Millis
  ): Either[String, Duration] =
    this match
      case Simple => DurationStyle.parseSimple(text, defaultUnit)
      case Iso8601 =>
        Try(Duration.parse(text)).toEither.left.map(error =>
          s"Invalid ISO-8601 duration '$text': ${error.getMessage}"
        )

  def print(value: Duration, unit: DurationStyle.DurationUnit): String =
    this match
      case Simple => unit.print(value)
      case Iso8601 => value.toString

object DurationStyle:
  enum DurationUnit(
      val suffix: String,
      val fromLong: Long => Duration,
      val toLong: Duration => Long
  ):
    case Nanos extends DurationUnit("ns", Duration.ofNanos, _.toNanos)
    case Micros extends DurationUnit(
      "us",
      value => Duration.ofNanos(Math.multiplyExact(value, 1000L)),
      _.toNanos / 1000L
    )
    case Millis extends DurationUnit("ms", Duration.ofMillis, _.toMillis)
    case Seconds extends DurationUnit("s", Duration.ofSeconds, _.getSeconds)
    case Minutes extends DurationUnit("m", Duration.ofMinutes, _.toMinutes)
    case Hours extends DurationUnit("h", Duration.ofHours, _.toHours)
    case Days extends DurationUnit("d", Duration.ofDays, _.toDays)

    def print(value: Duration): String = s"${toLong(value)}$suffix"

  object DurationUnit:
    def fromSuffix(suffix: String): Option[DurationUnit] =
      values.find(_.suffix == suffix)

  private val SimplePattern = "([+-]?\\d+)([a-zA-Z]{0,2})".r

  private def parseSimple(
      text: String,
      defaultUnit: DurationUnit
  ): Either[String, Duration] =
    text match
      case SimplePattern(numberText, suffix) =>
        for
          number <- Try(numberText.toLong).toEither.left.map(_ =>
            s"Invalid duration number '$numberText'"
          )
          unit <-
            if suffix.isEmpty then Right(defaultUnit)
            else DurationUnit.fromSuffix(suffix).toRight(s"Unknown duration suffix '$suffix'")
        yield unit.fromLong(number)

      case _ => Left(s"Invalid simple duration '$text'")

final case class Codec[A](
    parse: String => Either[String, A],
    print: A => String
)

object Codec:
  val text: Codec[String] = Codec(Right(_), identity)

  val integer: Codec[Int] = Codec(
    raw => Try(raw.trim.toInt).toEither.left.map(_ => s"Expected an integer, received '$raw'"),
    _.toString
  )

  val boolean: Codec[Boolean] = Codec(
    raw =>
      raw.trim.toLowerCase match
        case "true" => Right(true)
        case "false" => Right(false)
        case other => Left(s"Expected true or false, received '$other'"),
    _.toString
  )

  val duration: Codec[Duration] = Codec(
    DurationStyle.Iso8601.parse(_),
    _.toString
  )

enum ConfigKey[A](val name: String, val codec: Codec[A]):
  case ServiceName extends ConfigKey[String]("service.name", Codec.text)
  case RetryCount extends ConfigKey[Int]("service.retries", Codec.integer)
  case TlsEnabled extends ConfigKey[Boolean]("service.tls.enabled", Codec.boolean)
  case RequestTimeout extends ConfigKey[Duration]("service.timeout", Codec.duration)

def readConfig[A](
    source: Map[String, String],
    key: ConfigKey[A]
): Either[String, A] =
  source.get(key.name)
    .toRight(s"Missing configuration key '${key.name}'")
    .flatMap(key.codec.parse)

@main def durationAndConfigDemo(): Unit =
  println(DurationStyle.Simple.parse("30s"))
  println(DurationStyle.Iso8601.parse("PT30S"))
  println(DurationStyle.Simple.print(Duration.ofMinutes(5), DurationStyle.DurationUnit.Seconds))

  val configuration = Map(
    "service.name" -> "billing",
    "service.retries" -> "5",
    "service.tls.enabled" -> "true",
    "service.timeout" -> "PT15S"
  )

  val name: Either[String, String] = readConfig(configuration, ConfigKey.ServiceName)
  val retries: Either[String, Int] = readConfig(configuration, ConfigKey.RetryCount)
  val tls: Either[String, Boolean] = readConfig(configuration, ConfigKey.TlsEnabled)
  val timeout: Either[String, Duration] = readConfig(configuration, ConfigKey.RequestTimeout)

  println(name)
  println(retries)
  println(tls)
  println(timeout)
