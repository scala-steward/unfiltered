package unfiltered.porthole

import language.experimental.macros

import reflect.macros.Context

case class Porthole(obj: String, method: String, params: List[String])

object Porthole {
  def apply(param: Any): Porthole = macro applyImpl

  def applyImpl(c: Context)(param: c.Expr[Any]): c.Expr[Porthole] = {
    import c.universe._
    def mismatch() =
      c.abort(c.enclosingPosition,
        "Porthole mismatch:\nMust be called directly on a method, e.g. Porhole(MyObject.myMethod _)\n" +
          showRaw(param.tree)
      )

    val body = param.tree match {
      case Block(_, Function(vparams, body)) => body
      case _ => mismatch()
    }
    val (obj, method, params) = body match {
      case Apply(Select(Ident(name), method), params) => (name, method, params)
      case Apply(Select(Select(_,  name), method), params) => (name, method, params)
      case _ => mismatch()
    }
    val listApply = Select(reify(List).tree, newTermName("apply"))
    val names = params.map {
      case Ident(p) => c.literal(p.decoded).tree
    }

    val argList = c.Expr[List[String]](Apply(listApply, names))
    reify(Porthole(c.literal(obj.decoded).splice,
      c.literal(method.decoded).splice,
      argList.splice))
  }
}
