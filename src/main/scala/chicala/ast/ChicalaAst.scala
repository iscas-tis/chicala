package chicala.ast

import scala.tools.nsc
import nsc.Global

import chicala.util._

trait ChicalaAst
    extends CircuitInfos
    with CClassDefs
    with CStatements
    with CSignalInfos
    with CExps
    with SStatements
    with Printer
