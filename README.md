#### MerkleTree

In cryptography and computer science, a hash tree or Merkle tree is a tree in which every leaf node is labelled with the hash of a data block and every non-leaf node is labelled with the cryptographic hash of the labels of its child nodes. Hash trees allow efficient and secure verification of the contents of large data structures

https://en.wikipedia.org/wiki/Merkle_tree


#### Implementation

creating a tree is simple as providing a sequence of blocks and a digest function to create the hashes

```scala
def sha256(data:Block):Block = java.security.MessageDigest.getInstance("SHA-256").digest(data.toArray) 
val data:Seq[Block] = Seq(Seq(1,2,3), Seq(4,5,6), Seq(7,8,9))
val myTree = MerkleTree(data, sha256)

```
