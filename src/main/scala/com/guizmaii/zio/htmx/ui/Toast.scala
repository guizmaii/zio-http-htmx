package com.guizmaii.zio.htmx.ui

sealed trait Toast {
  def message: String
  def details: Option[String]
}
object Toast       {
  final case object None                                                            extends Toast {
    override def message: String         = ""
    override def details: Option[String] = Option.empty
  }
  final case class Primary(message: String, details: Option[String] = Option.empty) extends Toast
  final case class Success(message: String, details: Option[String] = Option.empty) extends Toast
  final case class Neutral(message: String, details: Option[String] = Option.empty) extends Toast
  final case class Warning(message: String, details: Option[String] = Option.empty) extends Toast
  final case class Error(message: String, details: Option[String] = Option.empty)   extends Toast
}
