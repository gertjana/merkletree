package merkletrees

import java.nio.file.{Files, Path, Paths}

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.io.{BufferedSource, Source}

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

    "read and convert a binary file into a merkletree" >> new TestScope {
      val path: Path = Paths.get(getClass.getResource("/random.bin").toURI)
      val byteArray: Array[Byte] = Files.readAllBytes(path)
      val blocks: Seq[Block] = byteArray.toSeq.grouped(1024).toSeq

      val testBlock: Block = blocks.iterator.drop(util.Random.nextInt(blocks.size-1)).next
      val merkleTree = MerkleTree(blocks)(sha256Hash)

      merkleTree.contains(sha256Hash(testBlock)) must beTrue
      merkleTree must haveClass[Branch]
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

    implicit val digestF:DigestF = sha256Hash

    val tree = MerkleTree(data)(sha256Hash)
    val same_tree = MerkleTree(data)(sha256Hash)

    val another_tree = MerkleTree(data2)(sha256Hash)
    val nonExistingHash: Block = sha256Hash(Seq(8,9,11))

  }



}
