package sensordata.daniel.probst.impl

import sensordata.daniel.probst.impl.DataFile.{AsciiSeparator, SignalValueIterator}
import zio.prelude.NonEmptyList

import java.io.*
import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.util.{Try, Using}

case class DataFile private (
  file: File,
  headerSizeInBytes: Long,
  numberOfSignalDescriptions: Int,
  signalDescriptions: NonEmptyList[SignalDescription],
  dataSetsCount: Long,
) {

  //region exportAsAscii

  def exportAsAscii(output: File): Try[Unit] = {
    Using.Manager { use =>
      val inputStream = use(FileInputStream(file))
      inputStream.skip(headerSizeInBytes)

      val bufferedWriter = use(BufferedWriter(FileWriter(output)))
      bufferedWriter.write(makeHeader)

      var continue = true
      while (continue) {
        makeLine(inputStream) match {
          case Some(line) =>
            bufferedWriter.newLine()
            bufferedWriter.write(line)
          case None => continue = false
        }
      }
    }
  }

  private def makeHeader: String = signalDescriptions.map(signalDescriptionToString).mkString(AsciiSeparator)

  private def signalDescriptionToString(signalDescription: SignalDescription) = {
    val unitOrEmptyString = signalDescription.unit.map(unit => s"unit: $unit, ").getOrElse("")
    s"${signalDescription.name}(${unitOrEmptyString}dataType: ${signalDescription.dataType})"
  }

  private def makeLine(stream: FileInputStream): Option[String] = {
    val stringBuilder = StringBuilder()

    @tailrec
    def iterate(remainingSignalDescriptions: NonEmptyList[SignalDescription]): Option[String] = {
      val signalValue = Util.readSignalValueOrThrow(stream, remainingSignalDescriptions.head) match {
        case Some(value) => value
        case None =>
          if (stringBuilder.isEmpty) {
            return None // end of well-formatted file
          } else { // this branch should not be reachable, since the number and correctness of all data sets is already validated when instantiating DataFile and calculating dataSetsCount
            val numberOfSignalValuesFound = signalDescriptions.length - remainingSignalDescriptions.length
            throw IllegalStateException(s"Last data set of file is incomplete - $numberOfSignalValuesFound signal values are present but ${signalDescriptions.length} signals are described")
          }
      }

      if (stringBuilder.nonEmpty) {
        stringBuilder.append(AsciiSeparator)
      }
      stringBuilder.append(signalValue)

      NonEmptyList.fromIterableOption(remainingSignalDescriptions.tail) match {
        case Some(newRemainingSignalDescriptions) => iterate(newRemainingSignalDescriptions)
        case None => Some(stringBuilder.result())
      }
    }

    iterate(signalDescriptions)
  }
  //endregion

  //region read

  def read(signalDescription: SignalDescription): SignalValueIterator = {
    val stream = FileInputStream(file)

    def dataSetBytesUntilSignal(signalDescriptions: List[SignalDescription]): Int = {
      signalDescriptions
        .takeWhile(_ != signalDescription)
        .map(_.dataType.sizeInBytes)
        .sum
    }

    def tryReadNextSignalValue(bytesToSkip: Long): Option[SignalValue] = {
      val bytesSkipped = stream.skip(bytesToSkip)
      if (bytesSkipped != bytesToSkip)
        None
      else
        Util.readSignalValueOrThrow(stream, signalDescription)
    }

    try {
      if (!signalDescriptions.exists(_ == signalDescription)) {
        throw IllegalArgumentException(s"Signal with name ${signalDescription.name} is not contained in the signal descriptions of this data file")
      }

      val dataSetBytesBeforeSignal = dataSetBytesUntilSignal(signalDescriptions)
      val dataSetBytesAfterSignal = dataSetBytesUntilSignal(signalDescriptions.reverse)

      val initialSkip = headerSizeInBytes + dataSetBytesBeforeSignal
      val skipBetweenDataSets = dataSetBytesBeforeSignal + dataSetBytesAfterSignal

      val firstSignalValue = tryReadNextSignalValue(initialSkip)
      new SignalValueIterator(signalDescription, stream, () => tryReadNextSignalValue(skipBetweenDataSets), firstSignalValue)

    } catch {
      case err =>
        stream.close()
        throw err
    }
  }
  //endregion
}

object DataFile {
  private val AsciiSeparator: String = "\t"

  def apply(file: File): Try[DataFile] = {
    Using(FileInputStream(file)) { stream =>
      val numberOfSignalDescriptions = Util.readLittleEndianIntOrThrow(stream)
      val signalDescriptions = List.fill(numberOfSignalDescriptions)(Util.readSignalDescriptionOrThrow(stream))
      if (signalDescriptions.length != signalDescriptions.distinct.length) throw IllegalStateException("Signal descriptions contain duplicates") // in a real application, this message might be improved to list all duplicates and their indices

      val headerSizeInBytes = stream.getChannel.position
      val remainingBytes = file.length - headerSizeInBytes
      val bytesPerDataSet = signalDescriptions.map(_.dataType.sizeInBytes).sum

      val dataSetsCount = remainingBytes / bytesPerDataSet
      val remainder = remainingBytes % bytesPerDataSet
      if (remainder != 0) throw IllegalStateException(s"Last data set of file is incomplete - $remainder bytes are present but $bytesPerDataSet bytes are expected")

      NonEmptyList.fromIterableOption(signalDescriptions) match {
        case Some(nonEmptySignalDescriptions) => new DataFile(file, headerSizeInBytes, numberOfSignalDescriptions, nonEmptySignalDescriptions, dataSetsCount)
        case None => throw IllegalStateException("File contains no signal descriptions")
      }
    }
  }

  class SignalValueIterator private[DataFile] (
    val signalDescription: SignalDescription,
    private val stream: FileInputStream,
    private val tryReadNextSignalValue: () => Option[SignalValue],
    private var nextSignalValue: Option[SignalValue],
  ) extends Iterator[SignalValue] with AutoCloseable {

    override def hasNext: Boolean = nextSignalValue.isDefined

    override def next(): SignalValue = {
      val currentSignalValue = nextSignalValue.getOrElse(throw NoSuchElementException("Next on empty iterator"))
      nextSignalValue = tryReadNextSignalValue() // if this throws an exception, currentSignalValue would be lost - in a real application, it would be better to catch the exception, save it in a temporary variable and only throw it once next() is called again
      currentSignalValue
    }

    override def close(): Unit = stream.close()
  }
}
