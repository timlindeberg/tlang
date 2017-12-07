package cafebabe

case class CodeFreezingException(message : String) extends Exception {
  override def getMessage : String = message
}
