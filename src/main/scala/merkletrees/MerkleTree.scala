package merkletrees

abstract class MerkleTree(val hash: Block) {
  def contains(hash: Block): Boolean
}

case class Leaf(override val hash: Block, data:Block) extends MerkleTree(hash) {
  override def contains(hash: Block): Boolean = this.hash == hash
}

case class Branch(override val hash: Block,
                  left: Option[MerkleTree] = Option.empty,
                  right: Option[MerkleTree] = Option.empty
                 ) extends MerkleTree(hash) {
  override def contains(hash: Block): Boolean = left.forall(_.contains(hash)) || right.forall(_.contains(hash))
}

object MerkleTree {
  def apply(blocks: Seq[Block], digestF: DigestF): MerkleTree = {
    assert(blocks.nonEmpty, "Can't do anything with an empty block list")

    var leaves = blocks.map(block => Leaf(digestF(block), block))

    def merge(digestF: DigestF, xs: Seq[MerkleTree]): MerkleTree = {
      xs.size match {
        case 1 =>
          Branch(digestF(xs.head.hash), xs.headOption, None)
        case 2 =>
          Branch(digestF(xs.head.hash ++ xs.tail.head.hash), xs.headOption, xs.tail.headOption)
      }
    }

    def loop(tree: Seq[MerkleTree]): Seq[MerkleTree] = {
      tree.size match {
        case 1 => tree
        case _ =>
          val grouped: Seq[MerkleTree] = tree.grouped(2).map(merge(digestF, _)).toSeq
          loop(grouped)
      }
    }
    loop(leaves).head
  }
}