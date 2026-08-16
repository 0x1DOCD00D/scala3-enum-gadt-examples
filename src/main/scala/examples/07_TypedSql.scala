package examples

import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.Types
import java.time.Instant

enum SqlType[A](val jdbcType: Int):
  case Int32 extends SqlType[Int](Types.INTEGER)
  case Text extends SqlType[String](Types.VARCHAR)
  case Bool extends SqlType[Boolean](Types.BOOLEAN)
  case InstantType extends SqlType[Instant](Types.TIMESTAMP)

  def bind(statement: PreparedStatement, index: Int, value: A): Unit =
    this match
      case Int32 => statement.setInt(index, value)
      case Text => statement.setString(index, value)
      case Bool => statement.setBoolean(index, value)
      case InstantType => statement.setTimestamp(index, java.sql.Timestamp.from(value))

  def readRequired(resultSet: ResultSet, columnIndex: Int): A =
    this match
      case Int32 => resultSet.getInt(columnIndex)
      case Text => resultSet.getString(columnIndex)
      case Bool => resultSet.getBoolean(columnIndex)
      case InstantType => resultSet.getTimestamp(columnIndex).toInstant

final case class Column[A](name: String, dataType: SqlType[A])

def bindColumn[A](
    statement: PreparedStatement,
    parameterIndex: Int,
    column: Column[A],
    value: A
): Unit =
  column.dataType.bind(statement, parameterIndex, value)

object TypedSqlExamples:
  val ageColumn: Column[Int] = Column("age", SqlType.Int32)
  val nameColumn: Column[String] = Column("name", SqlType.Text)
  val activeColumn: Column[Boolean] = Column("active", SqlType.Bool)

  // A real PreparedStatement is required to execute bindColumn.
  // The important point is that the value type is fixed by Column[A].
  //
  // bindColumn(statement, 1, ageColumn, 58)        // valid
  // bindColumn(statement, 1, ageColumn, "58")      // does not compile

@main def typedSqlDemo(): Unit =
  println(TypedSqlExamples.ageColumn)
  println(TypedSqlExamples.nameColumn)
  println(TypedSqlExamples.activeColumn)
