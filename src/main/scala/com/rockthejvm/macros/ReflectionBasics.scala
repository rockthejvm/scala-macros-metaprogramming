package com.rockthejvm.macros

import quoted.*

object ReflectionBasics {
  // instance.methodName(arg)
  inline def callMethodDynamically[A](instance: A, arg: Int, methodName: String): String = 
    ${ callMethodDynamicallyImpl('instance, 'arg, 'methodName) }

  def callMethodDynamicallyImpl[A: Type](instance: Expr[A], arg: Expr[Int], methodName: Expr[String])(using q: Quotes): Expr[String] = {
    // import the reflection package
    import q.reflect.*

    // Term = loosely type Expr = piece of an AST
    val term = instance.asTerm // the piece of AST that describes the instance

    // you can inspect Terms
    // Select = programmatic construction of a structure e.g. instance.method
    val method = Select.unique(term, methodName.valueOrAbort)

    // an Apply can build a method invocation
    val apply = Apply(method, List(arg.asTerm)) // instance.method(arg)

    // after we're done building the expression, we can turn it into the type we need
    apply.asExprOf[String]
  }

  // generate a tuple with N fields of type A
  // createTuple[3, Int] => (Int, Int, Int)
  transparent inline def createTuple[N <: Int, A](inline value: A) = 
    ${ createTupleImpl[N, A]('value) }

  def createTupleImpl[N <: Int : Type, A : Type](value: Expr[A])(using q: Quotes) = {
    import q.reflect.*

    inline def buildTupleSimple(n: Int): Expr[Tuple] = 
      if (n == 0) '{ EmptyTuple } 
      else '{ $value *: ${ buildTupleSimple(n - 1) } }

    def buildTupleExpr(tpe: Type[? <: AnyKind]): Expr[Tuple] =
      tpe match {
        case '[A *: rt] =>
          '{ $value *: ${ buildTupleExpr(TypeRepr.of[rt].asType).asExprOf[rt] } }
        case _ =>
          '{ EmptyTuple }
      }
    
    inline def buildTupleComplicated(n: Int): Expr[Tuple] = {
      // defn = package for meta-definitions in Scala
      // 1 - build the type constructor => TupleN
      val tupleConstructor = defn.TupleClass(n).typeRef // because isType returns true
      // 2 - build the type arguments
      val typeArguments = List.fill(n)(TypeRepr.of[A]) // list of all type parameters
      // 3 - build the full type TupleN[A, A, A, ...]
      val fullTupleType = AppliedType(tupleConstructor, typeArguments) // the full type TupleN[A, A, A...]
      // 4 - obtain the type descriptor Type[T]
      val actualType = fullTupleType.asType
      // 5 - build the actual tuple in an Expr
      buildTupleExpr(actualType)
    }

    // Type[N] => TypeRepr
    TypeRepr.of[N] match {
      case ConstantType(IntConstant(n)) if n > 1 => 
        buildTupleComplicated(n)

      case _ => report.errorAndAbort(s"I can't build a tuple out of the type ${Type.show[N]}")
    }
  }
}
