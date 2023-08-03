package chicala.ast

import scala.tools.nsc
import nsc.Global

import chicala.util._

trait ChicalaAst
    extends CircuitInfos
    with CClassDefs
    with CStatements
    with MTypes
    with CExps
    with SStatements
    with Printer
