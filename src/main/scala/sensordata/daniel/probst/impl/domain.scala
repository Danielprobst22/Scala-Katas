package sensordata.daniel.probst.impl

type SignalValue = Long | Double

case class SignalDescription(
  name: String,
  unit: Option[String],
  dataType: DataType,
)

enum DataType(val id: Int, val sizeInBytes: Int) {
  case Time extends DataType(id = 0, sizeInBytes = 8)
  case Double extends DataType(id = 1, sizeInBytes = 8)
}

object DataType {
  def of(id: Int): Option[DataType] = values.find(_.id == id)
}
