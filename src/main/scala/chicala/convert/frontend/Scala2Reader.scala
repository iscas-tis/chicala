package chicala.convert.frontend

import chicala.util._
import chicala.ast.ChicalaAst

trait Scala2Reader
    extends ChicalaAst
    // Loader
    with CClassDefsLoader
    with MStatementsLoader
    with MTermsLoader
    with CTermsLoader
    with STermsLoader
    with CTypesLoader
    with COpsLoader
    // TreeReader
    with ApplysReader
    with BlocksReader
    with DefDefsReader
    with IdentsReader
    with IfsReader
    with LiteralsReader
    with SelectsReader
    with StatementsReader
    with ValDefsReader
    // util
    with ChiselAstCheck
    with Printer
