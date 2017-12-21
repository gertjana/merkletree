package object merkletrees {
  type Block = Seq[Byte]
  type DigestF = (Block) => Block
}
