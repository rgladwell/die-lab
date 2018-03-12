package me.gladwell.tables.csv

import me.gladwell.tables.Table

case class Csv[R, C, T](rowHeader: String, table: Table[R, C, T]) {

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

  private val headers = (rowHeader +: table.columns.map(_.toString)).toList

  private def toRow(row: R) = {
    val rowData =
      for {
        column <- table.columns
      } yield table.get(column, row).getOrElse("").toString

    CsvRow(row.toString +: rowData)
  }

  private val dataRows =
    for {
      row <- table.rows
    } yield toRow(row)

  private val csv = CsvFile(CsvRow(headers) +: dataRows)

  override def toString = csv.toString

}
