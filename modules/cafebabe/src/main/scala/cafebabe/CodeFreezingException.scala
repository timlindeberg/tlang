package cafebabe

case class CodeFreezingException(message : String, stackTrace: Option[CodegenerationStackTrace] = None) extends Exception {
  override def getMessage : String = message
}
