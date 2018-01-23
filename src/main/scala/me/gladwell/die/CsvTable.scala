package me.gladwell.die

case class CsvTable[R, C](rows: Seq[R], rowHeader: String, columns: Seq[C], data: Map[(R, C), Any]) {

  private case class CsvRow(data: List[String]) {

    override def toString = data.map{ '"' + _ + '"'}.mkString(",")

  }

  private object CsvRow {

    def apply(data: Seq[String]): CsvRow = CsvRow(data.toList)

  }

  private case class CsvFile(rows: List[CsvRow]) {

    override def toString = rows.mkString(System.lineSeparator)

  }

  private object CsvFile {

    def apply(rows:  Seq[CsvRow]): CsvFile = CsvFile(rows.toList)

  }

  private val headers = (rowHeader +: columns.map(_.toString)).toList

  private def toRow(row: R) = {
    val rowData =
      for {
        column <- columns
      } yield data((row, column)).toString

    CsvRow(row.toString +: rowData)
  }

  private val dataRows =
    for {
      row <- rows
    } yield toRow(row)

  private val csv = CsvFile(CsvRow(headers) +: dataRows)

  override def toString = csv.toString

}
