package examples

import java.util.EnumMap
import java.util.EnumSet

enum JavaPermission extends java.lang.Enum[JavaPermission]:
  case Priority
  case Read
  case Write
  case Inherit

enum JavaRole extends java.lang.Enum[JavaRole]:
  case Admin
  case Producer
  case Consumer

@main def javaEnumCollectionsDemo(): Unit =
  val topicPermissions = EnumSet.of(JavaPermission.Read, JavaPermission.Write)
  println(topicPermissions.contains(JavaPermission.Read))

  val rolePermissions =
    new EnumMap[JavaRole, EnumSet[JavaPermission]](classOf[JavaRole])

  rolePermissions.put(JavaRole.Admin, EnumSet.allOf(classOf[JavaPermission]))
  rolePermissions.put(JavaRole.Producer, EnumSet.of(JavaPermission.Write))
  rolePermissions.put(JavaRole.Consumer, EnumSet.of(JavaPermission.Read))

  println(rolePermissions.get(JavaRole.Admin))
