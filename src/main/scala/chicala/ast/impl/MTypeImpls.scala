package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.MTypes

trait MTypeImpls { self: MTypes =>
  val global: Global
  import global._

  trait MTypeImpl { self: MType =>
    final def isSignalType = this match {
      case _: SignalType => true
      case _             => false
    }
    final def isSType = this match {
      case _: SType => true
      case _        => false
    }
  }
}
