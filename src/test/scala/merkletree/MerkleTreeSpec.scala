package merkletrees

import org.specs2.mutable.Specification

class MerkleTreeSpec extends Specification {

  def sha256Hash(data: Block): Block =
    java.security.MessageDigest.getInstance("SHA-256").digest(data.toArray)

  "a merkle tree should" >> {
    "create a tree from a seq of blocks" >> {
      val data = Seq(
        Seq[Byte](1,2,3),
        Seq[Byte](4,5,6),
        Seq[Byte](112,125,127)
      )
      val tree = MerkleTree(data, sha256Hash _)

      tree must haveClass[Branch]
      tree.asInstanceOf[Branch].left must haveClass[Some[Branch]]
      tree.asInstanceOf[Branch].right must haveClass[Some[Branch]]
      tree.asInstanceOf[Branch].left.asInstanceOf[Some[Branch]].get.left must haveClass[Some[Leaf]]
      tree.asInstanceOf[Branch].left.asInstanceOf[Some[Branch]].get.right must haveClass[Some[Leaf]]
      tree.asInstanceOf[Branch].right.asInstanceOf[Some[Branch]].get.left must haveClass[Some[Leaf]]
      tree.asInstanceOf[Branch].right.asInstanceOf[Some[Branch]].get.right must haveClass[None.type]
    }

    "creating two trees from the same data should have the same hash" >> {
      val data = Seq(
        Seq[Byte](1,2,3),
        Seq[Byte](4,5,6),
        Seq[Byte](112,125,127)
      )
      val tree = MerkleTree(data, sha256Hash _)
      val tree2 = MerkleTree(data, sha256Hash _)

      tree must haveClass[Branch]
      tree2 must haveClass[Branch]

      tree.hash === tree2.hash

    }

    "Creating two trees from different data should have a different hash" >> {
      val data = Seq(
        Seq[Byte](1,2,3),
        Seq[Byte](4,5,6),
        Seq[Byte](112,125,127)
      )
      val data2 = Seq(
        Seq[Byte](10,2,3),
        Seq[Byte](4,5,6),
        Seq[Byte](112,125,127)
      )
      val tree = MerkleTree(data, sha256Hash _)
      val tree2 = MerkleTree(data2, sha256Hash _)

      tree must haveClass[Branch]
      tree2 must haveClass[Branch]

      tree.hash !== tree2.hash
    }

    "be able to check if the tree contains a block with a certain hash" >> {
      val data = Seq(
        Seq[Byte](1,2,3),
        Seq[Byte](4,5,6),
        Seq[Byte](112,125,127)
      )
      val tree = MerkleTree(data, sha256Hash _)

    }
  }
}
