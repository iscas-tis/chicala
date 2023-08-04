package chicala.convert.frontend

import chicala.util._
import chicala.ast.ChicalaAst

trait Scala2Reader
    extends ChicalaAst
    with CClassDefsLoader
    with MStatementsLoader
    with CTypesLoader
    with COpsMatcher
    with StatementsReader
    with ValDefsReader
    with DefDefsReader
    with ApplysReader
    with BlocksReader
    with SelectsReader
    with ChiselAstCheck
    with Printer
