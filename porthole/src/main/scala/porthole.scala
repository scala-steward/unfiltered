package unfiltered.porthole

import language.experimental.macros

import reflect.macros.Context

object Porthole {
  def apply(param: Any): (String, List[String]) = macro applyImpl

  def applyImpl(c: Context)(param: c.Expr[Any]): c.Expr[(String,List[String])] = {
    import c.universe._
    def mismatch() =
      c.abort(c.enclosingPosition,
        "Porthole mismatch:\nMust be called directly on a method, e.g. Porhole(MyObject.myMethod _)")

    val body = param.tree match {
      case Function(vparams, body) => body
      case Block(_, Function(vparams, body)) => body
      case _ => mismatch()
    }
    val (methodName, params) = body match {
      case Apply(Select(obj, method), params) => (obj.toString + "." + method.decoded, params)
      case _ => mismatch()
    }
    val listApply = Select(reify(List).tree, newTermName("apply"))
    val names = params.map {
      case Ident(p) => c.literal(p.decoded).tree
    }

    val argList = c.Expr[List[String]](Apply(listApply, names))
    reify(c.literal(methodName).splice -> argList.splice)
  }
}
