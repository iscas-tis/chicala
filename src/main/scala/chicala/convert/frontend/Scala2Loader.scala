package chicala.convert.frontend

import chicala.util._
import chicala.ast.ChicalaAst

trait Scala2Loader
    extends ChicalaAst
    with CClassDefsLoader
    with MStatementsLoader
    with MTermsLoader
    with STermsLoader
    with MDefsLoader
    with CTypesLoader
    with COpsMatcher
    with ChiselAstCheck
    with Printer
