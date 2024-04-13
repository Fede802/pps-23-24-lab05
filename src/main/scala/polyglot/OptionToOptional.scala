package polyglot

import util.Optionals.Optional as ScalaOptional

import java.util.Optional
object OptionToOptional:
  def apply[A](option: ScalaOptional[A]): Optional[A] = option match
    case ScalaOptional.Just(a) => Optional.of(a)
    case _ => Optional.empty()
