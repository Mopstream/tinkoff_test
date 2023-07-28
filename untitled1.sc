import Main.Bytes
import scala.collection.mutable.ArrayBuffer

def calculateTable_CRC8(): Bytes = {
  val generator: Byte = 0x1D
  val crcTable: Bytes = ArrayBuffer.fill(256)(0)
  for (dividend <- 0 until 256) {
    var currByte = dividend
    for (_ <- 0 until 8) {
      if ((currByte & 0x80) != 0) {
        currByte <<= 1
        currByte ^= generator
      }
      else {
        currByte <<= 1
      }
    }
    crcTable(dividend) = currByte.toByte
  }
  crcTable
}

val crcTable: Bytes = calculateTable_CRC8()