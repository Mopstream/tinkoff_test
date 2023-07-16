import scala.collection.mutable.ArrayBuffer
import java.util.Base64


val src = "OAL_fwMCAQhTRU5TT1IwMQ8EDGQGT1RIRVIxD7AJBk9USEVSMgCsjQYGT1RIRVIzCAAGT1RIRVI03Q"
val a = Base64.getDecoder.decode(src.map { case '-' => '+'; case '_' => '/'; case c => c })
val b: ArrayBuffer[Int] = ArrayBuffer()

val c = b.find(x => x % 3 == 0)