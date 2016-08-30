package com.github.jancajthaml.jwt

private[jwt] class DeserializationException(message: String = null, cause: Throwable = null) extends
  RuntimeException(DeserializationException.defaultMessage(message, cause), cause)

private[jwt] object DeserializationException {
  def defaultMessage(message: String, cause: Throwable) =
    if (message != null) message
    else if (cause != null) cause.toString()
    else null
}
