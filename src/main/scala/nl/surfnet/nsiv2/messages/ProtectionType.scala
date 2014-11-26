package nl.surfnet.nsiv2.messages

/**
 * SURFnet specific connection protection type.
 */
sealed trait ProtectionType
object ProtectionType {
  case object PROTECTED extends ProtectionType
  case object UNPROTECTED extends ProtectionType
  case object REDUNDANT extends ProtectionType

  final val values: Seq[ProtectionType] = Vector(PROTECTED, UNPROTECTED, REDUNDANT)

  def fromString(s: String): Option[ProtectionType] = values.find { _.toString == s }
}
