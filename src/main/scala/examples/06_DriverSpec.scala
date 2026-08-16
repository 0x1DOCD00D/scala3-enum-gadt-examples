package examples

enum DriverSpec(
    val productNames: Set[String],
    val urlPrefixes: Set[String],
    val driverClassName: Option[String]
):
  case Unknown extends DriverSpec(Set.empty, Set.empty, None)
  case H2 extends DriverSpec(Set("H2"), Set("h2"), Some("org.h2.Driver"))
  case MySql extends DriverSpec(Set("MySQL"), Set("mysql"), Some("com.mysql.cj.jdbc.Driver"))
  case Hana extends DriverSpec(Set("HDB"), Set("sap"), Some("com.sap.db.jdbc.Driver"))
  case SqlServer extends DriverSpec(
    Set("Microsoft SQL Server", "SQL SERVER"),
    Set("sqlserver"),
    Some("com.microsoft.sqlserver.jdbc.SQLServerDriver")
  )
  case Firebird extends DriverSpec(
    Set("Firebird"),
    Set("firebirdsql", "firebird"),
    Some("org.firebirdsql.jdbc.FBDriver")
  )

object DriverSpec:
  def fromJdbcUrl(url: String): DriverSpec =
    values.iterator
      .filterNot(_ == Unknown)
      .find(driver => driver.urlPrefixes.exists(prefix => url.startsWith(s"jdbc:$prefix:")))
      .getOrElse(Unknown)

@main def driverSpecDemo(): Unit =
  println(DriverSpec.fromJdbcUrl("jdbc:firebird://localhost/example"))
