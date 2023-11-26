package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait STypeImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait STypeImpl { self: SType =>
    override def replaced(r: Map[String, MStatement]): SType = this
  }

  trait StTupleImpl { self: StTuple =>
    override def replaced(r: Map[String, MStatement]): StTuple =
      StTuple(tparams.map(_.replaced(r)))
  }

  trait StSeqImpl { self: StSeq =>
    override def replaced(r: Map[String, MStatement]): StSeq =
      StSeq(tparam.replaced(r))
  }
  trait StArrayImpl { self: StArray =>
    override def replaced(r: Map[String, MStatement]): StArray =
      StArray(tparam.replaced(r))
  }

  trait StWrappedImpl { self: StWrapped =>
    override def toString(): String = s"StWrapped(\"${str}\")"
  }

  trait KnownSizeObjImpl {
    def fromInt(size: Int): KnownSize = {
      KnownSize(SLiteral(size, StInt))
    }
  }
}
