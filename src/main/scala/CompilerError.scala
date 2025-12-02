package compiler

/**
 * Base domain exception that wraps runtime exceptions and other compiler errors.
 * Used as the left case of Either throughout the compiler.
 */
case class CompilerError(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull):
  override def toString: String = 
    cause match
      case Some(c) => s"CompilerError: $message (caused by: ${c.getMessage})"
      case None => s"CompilerError: $message"

object CompilerError:
  def fromThrowable(t: Throwable): CompilerError = 
    CompilerError(s"Runtime exception: ${t.getMessage}", Some(t))
  
  def fromRuntimeException(msg: String, t: Throwable): CompilerError =
    CompilerError(msg, Some(t))

