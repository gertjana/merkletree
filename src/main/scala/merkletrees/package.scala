package object merkletrees {
  type Block = Seq[Byte]
  type DigestF = (Block) => Block

  def b64(bytes: Block): String = java.util.Base64.getEncoder.encodeToString(bytes.toArray)

}
