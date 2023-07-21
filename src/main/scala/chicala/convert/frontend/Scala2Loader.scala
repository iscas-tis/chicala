package chicala.convert.frontend

import scala.tools.nsc
import nsc.Global

import chicala.util._
import chicala.ast._

trait Scala2Loader
    extends ChicalaAst
    with CClassDefsLoader
    with CStatementsLoader
    with CSignalInfosLoader
    with CExpsLoader
    with ChiselAstCheck
    with Printer
