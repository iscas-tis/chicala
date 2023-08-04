package chicala.convert.frontend

import chicala.util._
import chicala.ast.ChicalaAst

trait Scala2Loader
    extends ChicalaAst
    with CClassDefsLoader
    with StatementsLoader
    with CTypesLoader
    with COpsMatcher
    with ValDefsLoader
    with DefDefsLoader
    with ApplysLoader
    with BlocksLoader
    with SelectsLoader
    with ChiselAstCheck
    with Printer
