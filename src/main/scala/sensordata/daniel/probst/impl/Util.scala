package sensordata.daniel.probst.impl

import java.io.FileInputStream
import java.nio.{ByteBuffer, ByteOrder}

object Util {

  def readLittleEndianIntOrThrow(stream: FileInputStream): Int = {
    val bytes = Array.ofDim[Byte](4)
    val bytesRead = stream.read(bytes)
    if (bytesRead != 4) throw IllegalStateException("Failed to read integer - end of file reached unexpectedly")

    ByteBuffer
      .wrap(bytes)
      .order(ByteOrder.LITTLE_ENDIAN)
      .getInt
  }

  def readSignalDescriptionOrThrow(stream: FileInputStream): SignalDescription = {
    val signalName = readNullTerminatedStringOrThrow(stream).trim
    if (signalName.isEmpty) throw IllegalStateException("Signal name is empty")

    val rawUnit = readNullTerminatedStringOrThrow(stream).trim
    val unit = Option.when(rawUnit.nonEmpty)(rawUnit)

    val dataTypeId = readLittleEndianIntOrThrow(stream)
    val dataType = DataType.of(dataTypeId).getOrElse(throw IllegalStateException(s"DataType with id=$dataTypeId does not exist"))

    SignalDescription(signalName, unit, dataType)
  }

  private def readNullTerminatedStringOrThrow(stream: FileInputStream): String = {
    val stringBuilder = StringBuilder()
    var byte = stream.read()
    while (byte != 0) {
      if (byte == -1) throw IllegalStateException("Failed to read null-terminated string - end of file reached unexpectedly")

      stringBuilder.append(byte.toChar)
      byte = stream.read()
    }
    stringBuilder.result()
  }

  def readSignalValueOrThrow(stream: FileInputStream, signalDescription: SignalDescription): Option[SignalValue] = {
    val signalSize = signalDescription.dataType.sizeInBytes
    val bytes = Array.ofDim[Byte](signalSize)
    val bytesRead = stream.read(bytes)
    if (bytesRead == -1) {
      return None // end of stream
    } else if (bytesRead != signalSize) {
      throw IllegalStateException(s"Failed to read signal value - signal has size $signalSize but only $bytesRead bytes are present")
    }

    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN) // this ByteBuffer could theoretically be reused for performance reasons instead of allocating a new Array for every call of this function 
    val signalValue = signalDescription.dataType match {
      case DataType.Time => buffer.getLong
      case DataType.Double => buffer.getDouble
    }
    Some(signalValue)
  }
}
