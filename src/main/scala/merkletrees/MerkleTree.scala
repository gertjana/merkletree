package merkletrees

import java.util.Base64

import scala.annotation.tailrec

abstract class MerkleTree(val hash: Block) {
  def contains(hash: Block): Boolean
  protected def b64(bytes: Block): String = Base64.getEncoder.encodeToString(bytes.toArray)
}

case class Leaf(override val hash: Block, data:Block) extends MerkleTree(hash) {
  override def contains(hash: Block): Boolean = this.hash == hash
  override def toString = s"[L:${b64(hash.take(5))}]"
}

case class Branch(override val hash: Block,
                  left: Option[MerkleTree] = Option.empty,
                  right: Option[MerkleTree] = Option.empty
                 ) extends MerkleTree(hash) {
  override def contains(hash: Block): Boolean = left.exists(_.contains(hash)) || right.exists(_.contains(hash))
  override def toString = s"[B:${b64(hash.take(5))}:${left.toString}:${right.toString}]"
}

object MerkleTree {
  def apply(blocks: Seq[Block], digestF: DigestF): MerkleTree = {
    assert(blocks.nonEmpty, "Can't do anything with an empty block list")

    val leaves = blocks.map(block => Leaf(digestF(block), block))

    def merge(pair: Seq[MerkleTree], digestF: DigestF): MerkleTree = pair.size match {
      case 1 => Branch(digestF(pair.head.hash), pair.headOption, None)
      case 2 => Branch(digestF(pair.head.hash ++ pair.tail.head.hash), pair.headOption, pair.tail.headOption)
    }

    @tailrec
    def loop(tree: Seq[MerkleTree]): Seq[MerkleTree] = {
      tree.size match {
        case 1 => tree
        case _ => loop(tree.grouped(2).map(merge(_, digestF)).toSeq)
      }
    }
    loop(leaves).head
  }
}