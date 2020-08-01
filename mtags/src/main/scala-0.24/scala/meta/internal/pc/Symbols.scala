package scala.meta.internal.pc

import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Annotations.AnnotInfo

object Symbols {
  def isDeprecated(symbol: Symbol)(implicit ctx: Context) = symbol.isDeprecated
}