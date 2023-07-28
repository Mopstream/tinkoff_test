/*object solution {

  import java.util.Base64
  import java.nio.charset.StandardCharsets
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable
  import scala.annotation.tailrec
  import scala.sys.exit
  import scala.language.postfixOps

  import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
  import java.net.{HttpURLConnection, URL}

  private def sendHttpPostRequest(postData: String, url: String): (String, Int) = {
    val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("POST")
    connection.setDoOutput(true)

    val writer = new OutputStreamWriter(connection.getOutputStream)
    writer.write(postData)
    writer.flush()

    val responseCode = connection.getResponseCode

    val inputStream = connection.getInputStream
    val response = readInputStream(inputStream)

    writer.close()
    inputStream.close()
    connection.disconnect()

    (response, responseCode)
  }

  private def readInputStream(inputStream: java.io.InputStream): String = {
    val reader = new BufferedReader(new InputStreamReader(inputStream))
    val response = new StringBuilder
    var line: String = null

    while ( {
      line = reader.readLine(); line != null
    }) {
      response.append(line)
    }

    reader.close()
    response.toString
  }

  type Bytes = ArrayBuffer[Byte]

  private object Commands {
    val WhoIsHere: Byte = 0x01
    val IAmHere: Byte = 0x02
    val GetStatus: Byte = 0x03
    val Status: Byte = 0x04
    val SetStatus: Byte = 0x05
    val Tick: Byte = 0x06
  }

  object Util {
    def makeArr[T](bytes: Bytes, step: Bytes => (Option[T], Bytes)): ArrayBuffer[T] = {
      val arr: ArrayBuffer[T] = ArrayBuffer()

      @tailrec
      def help(bytes: Bytes): Unit = if (bytes.nonEmpty) {
        val (newElem, newBytes) = step(bytes)
        newElem match {
          case Some(elem) => arr += elem
          case _ =>
        }
        help(newBytes)
      }

      help(bytes)
      arr
    }

    val crcTable: Bytes = calculateTable_CRC8()

    private def calculateTable_CRC8(): Bytes = {
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

  }

  private object Timer {
    var currentTime: VarUInt = VarUInt(-1)

    def timeFrom(oldTime: VarUInt): VarUInt = currentTime - oldTime

  }

  object CoderBase64 {
    def decode(src: String): Bytes =
      ArrayBuffer() ++= Base64.getDecoder.decode(src.map { case '-' => '+'; case '_' => '/'; case c => c })

    def encode(src: Bytes): String =
      Base64.getEncoder.encodeToString(src.toArray).map { case '+' => '-'; case '/' => '_'; case c => c }
  }

  trait InBytesAble {
    def inBytes: Bytes

    def size: Byte = inBytes.length.toByte
  }

  case class MyString(value: String) extends InBytesAble {
    override def toString: String = value

    override def inBytes: Bytes = ArrayBuffer(value.length.toByte) ++= value.getBytes(StandardCharsets.UTF_8)

  }

  object MyString {
    def apply(bytes: Bytes): MyString = {
      new MyString(new String(bytes.tail.toArray, StandardCharsets.UTF_8))
    }
  }

  case class VarUInt(var value: BigInt) extends InBytesAble with Ordered[VarUInt] {
    override def toString: String = value.toString

    override def inBytes: Bytes = {
      val ret: Bytes = ArrayBuffer()

      @tailrec
      def help(num: BigInt): Unit = {
        val b = (num & 0x7f).toByte
        val newNum = num >> 7
        val x = if (newNum != 0) (b | 0x80).toByte else b
        ret += x
        if (newNum != 0) help(newNum)
      }

      help(value)
      ret
    }

    def ++ : VarUInt = {
      value += 1
      VarUInt(value - 1)
    }

    def -(b: VarUInt): VarUInt = VarUInt(value - b.value)

    def *(b: VarUInt): VarUInt = VarUInt(value * b.value)

    override def compare(that: VarUInt): Int = value.compare(that.value)
  }

  object VarUInt {
    def apply(bytes: Bytes): VarUInt = {
      @tailrec
      def help(bytes: Bytes, value: BigInt = 0, bitSize: Int = 0): BigInt = {
        if (bytes.nonEmpty) help(bytes.tail, value + (BigInt(bytes.head & 0x7f) << bitSize), bitSize + 7)
        else value
      }

      new VarUInt(help(bytes))
    }

    def apply(value: Int): VarUInt = VarUInt(BigInt(value))

    def searchFirstVar(bytes: Bytes): (VarUInt, Bytes) = {
      var cnt = 0
      val (valueBytes: Bytes, other: Bytes) = bytes.span(byte =>
        if (cnt >= 1) false
        else {
          if ((byte & 0x80) == 0) cnt += 1
          true
        })
      (VarUInt(valueBytes), other)
    }
  }

  case class Trigger(op: Byte, value: VarUInt, name: MyString) extends InBytesAble {
    override def toString: String = s"op = $op\nvalue = $value\nname = $name\n"

    override def inBytes: Bytes = op +: (value.inBytes ++ name.inBytes)
  }

  object Trigger {
    def apply(bytes: Bytes): ArrayBuffer[Trigger] =
      Util.makeArr(bytes, b => {
        val (value, other) = VarUInt.searchFirstVar(b.tail)
        val (nameBytes, rest) = other.splitAt(other(0) + 1)
        val name = MyString(nameBytes)
        (Some(Trigger(b.head, value, name)), rest)
      })
  }

  abstract class DevProps extends InBytesAble

  case class EnvSensorProps(sensors: Byte, triggers: ArrayBuffer[Trigger]) extends DevProps {
    override def toString: String = s"sensors = $sensors\ntriggers ={\n${triggers.mkString("\n")}}\n"

    override def inBytes: Bytes = sensors +: triggers.length.toByte +: triggers.flatMap(_.inBytes)
  }

  object EnvSensorProps {
    def apply(bytes: Bytes): EnvSensorProps = {
      val sensorProps = bytes(0)
      val triggers = Trigger(bytes.drop(2))
      new EnvSensorProps(sensorProps, triggers)
    }
  }

  class EmptyProps extends DevProps {
    override def toString: String = ""

    override def inBytes: Bytes = ArrayBuffer()
  }

  object EmptyProps {
    def apply(): EmptyProps = new EmptyProps()
  }

  class StringsProps(val values: ArrayBuffer[MyString]) extends DevProps {
    override def toString: String = s"values = [${values.mkString(" ")}]\n"

    override def inBytes: Bytes = values.length.toByte +: values.flatMap(_.inBytes)
  }

  object StringsProps {
    def apply(bytes: Bytes): StringsProps = {
      new StringsProps(Util.makeArr(bytes, b => {
        val (nameBytes, rest) = b.splitAt(b.head + 1)
        val name = MyString(nameBytes)
        (Some(name), rest)
      }))
    }

    def unapply(stringsProps: StringsProps): Option[ArrayBuffer[MyString]] = Some(stringsProps.values)
  }

  abstract class CmdBody extends InBytesAble

  case class Device(devName: MyString, devProps: DevProps) extends CmdBody {
    override def toString: String = s"devName = $devName\ndevProps = {\n$devProps}\n"

    override def inBytes: Bytes = devName.inBytes ++ devProps.inBytes
  }

  object Device {
    def apply(bytes: Bytes): Device = {
      val devType = bytes(0)
      val (nameBytes, propsBytes) = bytes.drop(2).splitAt(bytes(2) + 1)
      val name = MyString(nameBytes)
      val props = devType match {
        case 2 => EnvSensorProps(propsBytes)
        case 3 => StringsProps(propsBytes.tail)
        case _ => EmptyProps()
      }
      new Device(name, props)
    }

    val SmartHub: Byte = 0x01
    val EnvSensor: Byte = 0x02
    val Switch: Byte = 0x03
  }

  case class TimerCmdBody(timestamp: VarUInt) extends CmdBody {
    override def toString: String = s"timestamp = $timestamp\n"

    override def inBytes: Bytes = timestamp.inBytes
  }

  object TimerCmdBody {

    def apply(bytes: Bytes): TimerCmdBody = {
      val time = VarUInt(bytes)
      new TimerCmdBody(time)
    }
  }

  class EnvSensorStatus(val values: ArrayBuffer[VarUInt]) extends CmdBody {
    override def toString: String = s"values = [${values.mkString(" ")}]\n"

    override def inBytes: Bytes = values.flatMap(_.inBytes)
  }

  object EnvSensorStatus {
    def apply(bytes: Bytes): EnvSensorStatus =
      new EnvSensorStatus(Util.makeArr(bytes, b => {
        val (int, rest) = VarUInt.searchFirstVar(b)
        (Some(int), rest)
      }))

    def unapply(envSensorStatus: EnvSensorStatus): Option[ArrayBuffer[VarUInt]] = Some(envSensorStatus.values)
  }

  case class ValueBody(value: Byte) extends CmdBody {
    override def toString: String = s"value = $value\n"

    override def inBytes: Bytes = ArrayBuffer(value)
  }

  object ValueBody {
    def apply(bytes: Bytes): ValueBody = new ValueBody(bytes(0))
  }

  class EmptyBody extends CmdBody {
    override def toString: String = ""

    override def inBytes: Bytes = ArrayBuffer()
  }

  object EmptyBody {
    def apply(): EmptyBody = new EmptyBody()
  }

  case class Payload(src: VarUInt, dst: VarUInt, serial: VarUInt, devType: Byte, cmd: Byte, cmdBody: CmdBody) extends InBytesAble {
    override def toString: String = s"src = $src\ndst = $dst\nserial = $serial\ndevType = $devType\ncmd = $cmd\ncmdBody = {\n$cmdBody}\n"

    override def inBytes: Bytes =
      src.inBytes ++ dst.inBytes ++ serial.inBytes ++ ArrayBuffer(devType, cmd) ++ cmdBody.inBytes

    val crc8: Byte = {
      var crc: Byte = 0
      inBytes.foreach(b => {
        val data = b ^ crc
        val fixed_data = data + (if (data < 0) 256 else 0)
        crc = Util.crcTable(fixed_data)
      })
      crc
    }
  }

  object Payload {
    def apply(bytes: Bytes): Payload = {
      val (src, rest) = VarUInt.searchFirstVar(bytes)
      val (dst, rest1) = VarUInt.searchFirstVar(rest)
      val (serial, others) = VarUInt.searchFirstVar(rest1)
      val devType = others(0)
      val cmd = others(1)
      val other = others.drop(2)
      val cmdBody: CmdBody = (cmd, devType) match {
        case (Commands.Status, Device.EnvSensor) => EnvSensorStatus(other)
        case (Commands.WhoIsHere, _) | (Commands.IAmHere, _) => Device(others)
        case (Commands.Status, _) | (Commands.SetStatus, _) => ValueBody(other)
        case (Commands.GetStatus, _) => EmptyBody()
        case (Commands.Tick, _) => TimerCmdBody(other)
      }
      new Payload(src, dst, serial, devType, cmd, cmdBody)
    }
  }

  case class Packet(length: Byte, payload: Payload, crc8: Byte) extends InBytesAble {
    override def toString: String = s"[\nlen = $length\npayload = {\n$payload}\ncrc8 = $crc8\n]\n"

    override def inBytes: Bytes = length +: (payload.inBytes :+ crc8)

    def encode: String = CoderBase64.encode(inBytes).takeWhile(_ != '=')
  }

  object Packet {
    def apply(bytes: Bytes): Packet = {
      val length = bytes.head
      val payload = Payload(bytes.tail.init)
      val crc8 = bytes.last
      new Packet(length, payload, crc8)
    }

    def apply(encoded: String): Option[ArrayBuffer[Packet]] = {
      try {
        val bytes: Bytes = CoderBase64.decode(encoded)
        val arr = Util.makeArr(bytes, b => {
          val (packet, rest) = b.splitAt(b(0) + 2)
          try {
            val p = Packet(packet)
            if (p.payload.crc8 == p.crc8) {
              (Some(p), rest)
            }
            else (None, rest)
          } catch {
            case _: Throwable => (None, rest)
          }
        })
        Some(arr)
      } catch {
        case _: Throwable => None
      }
    }

    def apply(payload: Payload): Packet = {
      new Packet(payload.size, payload, payload.crc8)
    }
  }

  def main(args: Array[String]): Unit = {
      val url = args(0)
      val broadcast = VarUInt(0x3FFF)


      val myAddress = VarUInt(BigInt(args(1), 16))
      val mySerial = VarUInt(1)
      val myName = MyString("HUB01")
      val myDevice = Device(myName, EmptyProps())


      val requestQueue: ArrayBuffer[(Byte, VarUInt, VarUInt)] = ArrayBuffer()
      val devicesMap: mutable.Map[MyString, (VarUInt, DevProps, Byte)] = mutable.Map()
      val namesMap: mutable.Map[VarUInt, MyString] = mutable.Map()
      val whoIsHere = Packet(Payload(myAddress, broadcast, mySerial ++, Device.SmartHub, Commands.WhoIsHere, myDevice))
      var requestBody = whoIsHere.encode


      while (true) {
        val response = sendHttpPostRequest(requestBody, url)
        response match {
          case (body, 200) => requestBody = handle(body)
          case (_, 204) => exit(0)
          case _ => exit(99)
        }

        Thread.sleep(2000)
        println("\n\nNEXT STEP\n\n")
      }

      def handle(value: String): String = {
        var request: String = ""

        timeoutCleaner()
        for {
          arr <- Packet(value)
          packet <- arr
        } {
          val currPayload = packet.payload
          val currCmdBody = currPayload.cmdBody
          val currSrc = currPayload.src

          request += (
            currPayload.cmd match {
              case Commands.WhoIsHere => print("WhoIsHere\t"); whoIsHereHandler()
              case Commands.IAmHere => print("IAmIsHere\t"); iAmHereHandler()
              case Commands.Status => print("Status\t"); statusHandler()
              case Commands.Tick => print("Tick\t"); tickHandler()
              case _ => ""
            })

          def whoIsHereHandler(): String = {
            rememberIt()
            Packet(Payload(myAddress, broadcast, mySerial ++, Device.SmartHub, Commands.IAmHere, myDevice)).encode
          }

          def iAmHereHandler(): String = {
            if (requestQueue.exists(_._1 == Commands.WhoIsHere)) {
              rememberIt()
              if (currPayload.devType != Device.EnvSensor) requestQueue += ((Commands.GetStatus, Timer.currentTime, currSrc))
              Packet(Payload(myAddress, currSrc, mySerial ++, currPayload.devType, Commands.GetStatus, EmptyBody())).encode
            } else {
              forgetIt()
              ""
            }
          }

          def statusHandler(): String = {
            requestQueue.find(x => (x._3 == currSrc) && (x._1 == Commands.GetStatus || x._1 == Commands.SetStatus)) match {
              case Some(x) => requestQueue -= x
              case _ =>
            }
            currCmdBody match {
              case EnvSensorStatus(values) => manageSensor(values, namesMap(currSrc))
              case ValueBody(value) if currPayload.devType == Device.Switch => manageSwitch(value)
              case _ => ""
            }
          }

          def tickHandler(): String = {
            currCmdBody match {
              case TimerCmdBody(timestamp) =>
                if (Timer.currentTime == VarUInt(-1)) requestQueue += ((Commands.WhoIsHere, timestamp, broadcast))
                Timer.currentTime = timestamp
            }
            ""
          }

          def rememberIt(): Unit = {
            currCmdBody match {
              case Device(currName, currProps) =>
                namesMap(currSrc) = currName
                devicesMap(currName) = (currSrc, currProps, currPayload.devType)
            }
          }

          def forgetIt(): Unit = {
            currCmdBody match {
              case Device(currName, _) =>
                namesMap -= currSrc
                devicesMap -= currName
            }
          }

          def manageSwitch(value: Byte): String = {
            var mes = ""
            val props = devicesMap(namesMap(currSrc))._2
            props match {
              case StringsProps(values) =>
                for (name <- values) {
                  mes += switch(name, value)
                }
            }
            mes
          }

          def manageSensor(values: ArrayBuffer[VarUInt], name: MyString): String = {
            val (_, props, _) = devicesMap(name)
            var mes = ""
            props match {
              case EnvSensorProps(sensors, triggers) =>
                val sensorValues: Map[Int, VarUInt] = ArrayBuffer(1, 2, 3, 4).filter(x => (sensors & 0xF & x) != 0).zip(values).toMap
                for (Trigger(op, value, name) <- triggers) {
                  val switchOn = (op & 1).toByte
                  val isTrue = op & 2
                  val sensorNum = op & 12
                  mes += switch(name, switchOn)
                  if (((sensorValues(sensorNum) - value) * VarUInt(isTrue)) > VarUInt(0)) devicesMap(name)
                }
            }
            mes
          }

          def switch(name: MyString, value: Byte) = {
            val (dst, _, devType) = devicesMap(name)
            Packet(Payload(myAddress, dst, mySerial ++, devType, Commands.SetStatus, ValueBody(value))).encode
          }

        }

        request
      }

      def timeoutCleaner(): Unit = {
        val toRemove: ArrayBuffer[(Byte, VarUInt, VarUInt)] = ArrayBuffer()
        for (expected <- requestQueue) {
          if (Timer.timeFrom(expected._2) >= VarUInt(300)) {
            toRemove += expected
            if (expected._3 != broadcast) {
              devicesMap -= namesMap(expected._3)
              namesMap -= expected._3
            }
          }
        }
        requestQueue --= toRemove
      }
  }
}
*/