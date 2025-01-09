package com.rockthejvm.wartimizer

import quoted.*
import scala.reflect.NameTransformer

/* 
  Code processor that will transform code for ONE use-case
  Example (code optimization)
  before: List(1,2,3).filter(_ % 2 == 0).headOption
  after: List(1,2,3).find(_ % 2 == 0)

  Example (wart remover)
  before: "Scala is " + Person("Martin Odersky", "martin@gmail.com")
  after: (should not compile)

  1. Write the Wartimizer impls => compile the instances
  2. Wartimizer.wartimize(w1, w2, w3, ...)(myCode)
    - wartimizeImpl will fetch the Wartimization instances by their NAME from the classpath
    - call their treeMap functions
    - the treeMaps will run on the 'myCode
    - will return a new expression
  3. Continue compiling

 */
trait Wartimization { self: Singleton => // all Wartimization instances must be objects, so that they can be referred to as constants
  def treeMap(using q: Quotes): q.reflect.TreeMap
}

object Wartimization {
  given FromExpr[Wartimization] with {
    /* 
      will fetch the Wartimization object from the classpath by its name
      => will use RUNTIME reflection (!)
     */
    def unapply(x: Expr[Wartimization])(using q: Quotes): Option[Wartimization] = {
      import q.reflect.*
      // get the name of the Wartimization instance
      val typeSymbol = x.asTerm.tpe.typeSymbol

      if (typeSymbol.flags.is(Flags.Module)) { // must check that this Wartimization is an object
        val fullName = typeSymbol.fullName // fully qualified class name of the object
        Some(unsafeLoadObject(fullName))
      } else {
        report.errorAndAbort(s"The expression ${x.show} of type ${typeSymbol.name} does not correspond to an compile-time constant object.", x)
      }
    }

    private def unsafeLoadObject[A](name: String)(using q: Quotes): A = {
      import q.reflect.*

      try {

        val clazz = Class.forName(name)
        // objects are represented in the JVM as a static field called MODULE$
        val module = clazz.getField(NameTransformer.MODULE_INSTANCE_NAME)
        // this Field is normally fetched from a class instance, here we don't have one
        val objectInstance = module.get(null) // the object is static
        // final value
        objectInstance.asInstanceOf[A]
      } catch {
        case e: Throwable =>
          report.errorAndAbort(s"""
            Failed to load class [$name].
            Make sure that:
              - it is a top-level object (or nested in another object)
              - it is defined in a file separate from where this macro is being invoked
              - it is being referred to directly as the object rather than an alias or val
          """)
      }
    }
  }
}
