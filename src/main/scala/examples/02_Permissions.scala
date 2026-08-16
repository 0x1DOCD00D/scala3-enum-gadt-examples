package examples

enum Permission:
  case Priority
  case Read
  case Write
  case Inherit

enum Role:
  case Admin
  case Producer
  case Consumer

val permissionsByRole: Map[Role, Set[Permission]] =
  Map(
    Role.Admin -> Permission.values.toSet,
    Role.Producer -> Set(Permission.Write),
    Role.Consumer -> Set(Permission.Read)
  )

final case class AccessDenied(role: Role, permission: Permission)

def requirePermission(
    role: Role,
    permission: Permission
): Either[AccessDenied, Unit] =
  val granted = permissionsByRole.getOrElse(role, Set.empty)
  if granted.contains(permission) then Right(())
  else Left(AccessDenied(role, permission))

sealed trait Capability
sealed trait CanRead extends Capability
sealed trait CanWrite extends Capability

enum PermissionTag[P <: Capability](val runtimePermission: Permission):
  case Read extends PermissionTag[CanRead](Permission.Read)
  case Write extends PermissionTag[CanWrite](Permission.Write)

final class Permit[P <: Capability] private ()

object Permit:
  def authorize[P <: Capability](
      role: Role,
      requested: PermissionTag[P],
      table: Map[Role, Set[Permission]]
  ): Either[AccessDenied, Permit[P]] =
    val permissions = table.getOrElse(role, Set.empty)
    if permissions.contains(requested.runtimePermission) then Right(new Permit[P]())
    else Left(AccessDenied(role, requested.runtimePermission))

enum BrokerCommand[P <: Capability, A]:
  case Read(topic: String) extends BrokerCommand[CanRead, Vector[String]]
  case Publish(topic: String, message: String) extends BrokerCommand[CanWrite, Long]

def execute[P <: Capability, A](
    command: BrokerCommand[P, A]
)(using Permit[P]): A =
  command match
    case BrokerCommand.Read(topic) =>
      Vector(s"message-1 from $topic", s"message-2 from $topic")

    case BrokerCommand.Publish(topic, message) =>
      math.abs((topic, message).hashCode.toLong)

@main def permissionDemo(): Unit =
  println(requirePermission(Role.Consumer, Permission.Read))
  println(requirePermission(Role.Consumer, Permission.Write))

  Permit.authorize(Role.Consumer, PermissionTag.Read, permissionsByRole) match
    case Right(permit) =>
      given Permit[CanRead] = permit
      val messages: Vector[String] = execute(BrokerCommand.Read("orders"))
      println(messages)

      // Does not compile because Permit[CanWrite] is missing.
      // val id = execute(BrokerCommand.Publish("orders", "new order"))

    case Left(error) =>
      println(error)
