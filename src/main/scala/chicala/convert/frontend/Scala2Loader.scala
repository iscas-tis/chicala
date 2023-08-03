package chicala.convert.frontend

import chicala.util._
import chicala.ast.ChicalaAst

trait Scala2Loader
    extends ChicalaAst
    with CClassDefsLoader
    with MStatementsLoader
    with CTypesLoader
    with CExpsLoader
    with SStatementsLoader
    with ChiselAstCheck
    with Printer
