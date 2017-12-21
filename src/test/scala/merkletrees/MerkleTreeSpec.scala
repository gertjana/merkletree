package merkletrees

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class MerkleTreeSpec extends Specification {

  "a merkle tree should" >> {
    "create a tree from a seq of blocks" >> new TestScope {
      tree must haveClass[Branch]
      tree.asInstanceOf[Branch].left must haveClass[Some[Branch]]
      tree.asInstanceOf[Branch].right must haveClass[Some[Branch]]
      tree.asInstanceOf[Branch].left.asInstanceOf[Some[Branch]].get.left must haveClass[Some[Leaf]]
      tree.asInstanceOf[Branch].left.asInstanceOf[Some[Branch]].get.right must haveClass[Some[Leaf]]
      tree.asInstanceOf[Branch].right.asInstanceOf[Some[Branch]].get.left must haveClass[Some[Leaf]]
      tree.asInstanceOf[Branch].right.asInstanceOf[Some[Branch]].get.right must haveClass[None.type]
    }

    "creating two trees from the same data should have the same hash" >> new TestScope {
      tree must haveClass[Branch]
      same_tree must haveClass[Branch]

      tree.hash === same_tree.hash

    }

    "Creating two trees from different data should have a different hash" >> new TestScope {
      tree must haveClass[Branch]
      another_tree must haveClass[Branch]
      tree.hash !== another_tree.hash
    }

    "be able to check if the tree contains a block with a certain hash" >> new TestScope {
      data.map(sha256Hash).map(
        b => tree.contains(b) must beTrue
      )
    }
    "contains should return false for a non existing hash" >> new TestScope {
      tree.contains(nonExistingHash) must beFalse
    }
  }

  trait TestScope extends Scope {
    def sha256Hash(data: Block): Block = java.security.MessageDigest.getInstance("SHA-256").digest(data.toArray)

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

    val tree = MerkleTree(data, sha256Hash)
    val same_tree = MerkleTree(data, sha256Hash)

    val another_tree = MerkleTree(data2, sha256Hash)
    val nonExistingHash: Block = sha256Hash(Seq(8,9,11))

  }
}
