package examples

import java.util.Locale

import java.util.Locale

enum DatabaseDriver(
                     val productName: Option[String],
                     val driverClassName: Option[String]
                   ):
  case Unknown
    extends DatabaseDriver(
      None,
      None
    )

  case H2
    extends DatabaseDriver(
      Some("H2"),
      Some("org.h2.Driver")
    )

  case MySql
    extends DatabaseDriver(
      Some("MySQL"),
      Some("com.mysql.cj.jdbc.Driver")
    )

  case Hana
    extends DatabaseDriver(
      Some("HDB"),
      Some("com.sap.db.jdbc.Driver")
    )

  case SqlServer
    extends DatabaseDriver(
      Some("Microsoft SQL Server"),
      Some("com.microsoft.sqlserver.jdbc.SQLServerDriver")
    )

  case Firebird
    extends DatabaseDriver(
      Some("Firebird"),
      Some("org.firebirdsql.jdbc.FBDriver")
    )

  def urlPrefixes: List[String] =
    this match
      case Hana =>
        List("sap")

      case Firebird =>
        List(
          "firebirdsql",
          "firebird"
        )

      case _ =>
        List(
          toString.toLowerCase(Locale.ROOT)
        )

  def productMatches(actualName: String): Boolean =
    this match
      case SqlServer =>
        productName.exists(
          _.equalsIgnoreCase(actualName)
        ) ||
          actualName.equalsIgnoreCase(
            "SQL SERVER"
          )

      case _ =>
        productName.exists(
          _.equalsIgnoreCase(actualName)
        )

object DatabaseDriver:

  def fromJdbcUrl(url: String): DatabaseDriver =
    values.iterator
      .filterNot(_ == Unknown)
      .find { driver =>
        driver.urlPrefixes.exists { prefix =>
          url.startsWith(
            s"jdbc:$prefix:"
          )
        }
      }
      .getOrElse(Unknown)

  def fromProductName(
                       productName: String
                     ): DatabaseDriver =
    values.iterator
      .filterNot(_ == Unknown)
      .find(
        _.productMatches(productName)
      )
      .getOrElse(Unknown)

@main def databaseDriverDemo(): Unit =
  val mysql =
    DatabaseDriver.fromJdbcUrl(
      "jdbc:mysql://localhost/store"
    )

  val hana =
    DatabaseDriver.fromJdbcUrl(
      "jdbc:sap://localhost/system"
    )

  val firebird =
    DatabaseDriver.fromJdbcUrl(
      "jdbc:firebirdsql://localhost/database"
    )

  val sqlServer =
    DatabaseDriver.fromProductName(
      "SQL SERVER"
    )

  val unknown =
    DatabaseDriver.fromJdbcUrl(
      "jdbc:mystery://localhost/db"
    )

  println(mysql)
  println(hana)
  println(firebird)
  println(sqlServer)
  println(unknown)