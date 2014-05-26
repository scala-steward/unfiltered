package unfiltered.porthole

import language.experimental.macros

import reflect.macros.Context

object Porthole {
  def apply(param: Any): List[String] = macro applyImpl

  def applyImpl(c: Context)(param: c.Expr[Any]): c.Expr[List[String]] = {
    import c.universe._
    val (vparams, body) = param.tree match {
      case Function(vparams, body) => (vparams, body)
      case Block(_, Function(vparams, body)) => (vparams, body)
      case _ => c.abort(c.enclosingPosition, 
        "Porthole mismatch:\nMust be called directly on a method, e.g. Porhole(MyObject.myMethod _)")
    }
    val funcName = body match {
      case Apply(s) => s
    }
    val listApply = Select(reify(List).tree, newTermName("apply"))

    val names = vparams.map( p => c.literal(p.name.decoded).tree)

    c.Expr[List[String]](Apply(listApply, names))
  }
}
