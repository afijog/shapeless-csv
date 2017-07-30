package es.massimple.csv

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, DateTimeFormat}
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.util.Try

trait CsvParser[T] {
  import CsvParser.{Cell, ParseResult}
  def parse(cells: List[Cell]): ParseResult[T]
  def to(t: T): String
  /**
    * The number of `Cell`s this `CsvParser` needs to consume to produce a `T`
    */
  def size: Int
  val header: List[String]

  /**
    * Attempt to parse the CSV string `csv` into a `T`
    *
    * Example usage:
    * {{{
    * // Produces Right(3)
    * CsvParser[Int].parse("3")
    *
    * case class Sample(i: Int)
    * // Produces Right(Sample(3))
    * CsvParser[Sample].parse("3")
    * }}}
    *
    * @param csv A string representation of a CSV. Each cell must be divided by a comma and each line must be separated by a newline
    * @param hasHeader Does `csv` have a header? Defaults to false (i.e. no)
    */
  def parse(csv: String, hasHeader: Boolean = false): List[ParseResult[T]] = {
    val allLines: List[Cell] = csv.split("\n").toList
    val lines = if (hasHeader) allLines.tail else allLines
    lines.map { line =>
      val cells = line.split("," , -1).toList
      parse(cells)
    }
  }
  def parseSkipHeader(csv: String)(implicit parser: CsvParser[T]): List[ParseResult[T]] = parse(csv, true)
}

object CsvParser {
  implicit val dtfDate: DateTimeFormatter = DateTimeFormat.forPattern("YYYY-MM-dd")

  def apply[T](implicit parser: CsvParser[T]) : CsvParser[T] = parser

  type Cell = String
  type ParseResult[T] = Either[String, T]
  type Stringifier[T] = T => String

  implicit val stringParser = primitiveParser(identity)
  implicit val charParser = primitiveParser { s =>
    if (s.length == 1) s.head
    else throw new IllegalArgumentException("Char Parser requires strings of size one")
  }
  implicit val byteParser = primitiveParser(_.toByte)
  implicit val shortParser = primitiveParser(_.toShort)
  implicit val intParser = primitiveParser(_.toInt)
  implicit val longParser = primitiveParser(_.toLong)
  implicit val floatParser = primitiveParser(_.toFloat)
  implicit val doubleParser = primitiveParser(_.toDouble)
  implicit val booleanParser = primitiveParser(_.toBoolean)

  def jodaDateParserBuilder(implicit dtf: DateTimeFormatter) = primitiveParser[DateTime]( dtf.parseDateTime(_).withTimeAtStartOfDay(), dtf.print(_) )
  implicit val jodaDateParser = jodaDateParserBuilder

  /**
    * Create a `CsvParser` that consumes one cell and attempts to produce a primitive of type `Option[T]`
    * @param parser A CsvParser[T] to delegate the logic of type T
    * @param typ The Typeable case class to get info about the type T
    * @return
    */
  implicit def optionParser[T](implicit parser: CsvParser[T], typ: Typeable[T]) : CsvParser[Option[T]] = new CsvParser[Option[T]] {
    def parse(cells: List[Cell]): ParseResult[Option[T]] = {
      if (cells.size != size) Left(s"Cannot parse [${cells.mkString(",")}] to Option[${typ.describe}]")
      else if (cells.forall { _.isEmpty}) Right(None)
      else {
        val maybeParsed: ParseResult[T] = parser.parse(cells)
        maybeParsed.right.map(Option(_))
      }
    }
    def size = parser.size
    val emptyRepr : String = (1 to size).map(_ => "").mkString(",")
    def to(t: Option[T]): String = t.map(parser.to).getOrElse(emptyRepr)
    val header = parser.header
  }

  def defaultStringifier[T] : Stringifier[T] = s => s.toString

  /**
    * Create a `CsvParser` that consumes one cell and attempts to produce a primitive of type `T`
    * @param parse The function to parse a `Cell` into `T`
    * @param typ The typeclass used to find type information for `T`
    */
  def primitiveParser[T](parse: Cell => T, stringifier: Stringifier[T] = defaultStringifier[T])
                        (implicit typ: Typeable[T]) : CsvParser[T] = {
    headInstance(s => Try(parse(s)).toOption)(s => s"Cannot parse [$s] to ${typ.describe}")(stringifier = stringifier)
  }

  /**
    * Create a `CsvParser` that consumes a single cell and parses it into an instance of `T`
    * @param p The function that attempts to parse a cell into an instance of `T` consuming 1 cell
    * @param failure The function used to create an error method. The input to the method
    * will be the cell(s) that could not be parsed.
    * @param stringifier The function that converts a T to a string representation
    * @return A CsvParser[T] instance
    */
  def headInstance[T](p: Cell => Option[T])(failure: String => String)(stringifier: Stringifier[T]): CsvParser[T] = new CsvParser[T] {
    def parse(cells: List[Cell]): ParseResult[T] = {
      cells match {
        case head +: Nil =>
          val maybeParsed = p(head)
          maybeParsed.toRight(failure(head))
        case _ =>
          Left(failure(cells.mkString(",")))
      }
    }
    def size = 1
    def to(t: T): String = stringifier(t)
    val header = List.empty
  }

  /**
    * Create a `CsvParser` that consumes `s` number of cells and attempts to produce an instance of `T`
    * @param s The number of `Cell`s this `CsvParser` needs to consume to produce a `T` consuming s cells
    * @param p The function that attempts to parse a cell into an instance of `T`
    * @param stringifier The function that converts a T to a string representation
    * @param toHeader The function that retrieves the header representation for a T
    * @return A CsvParser[T] instance
    */
  def instance[T](s: Int)(p: List[Cell] => ParseResult[T],
                          stringifier: Stringifier[T],
                          toHeader: List[String] = List.empty): CsvParser[T] = new CsvParser[T] {
    def parse(cells: List[Cell]): ParseResult[T] = {
      if (cells.length == s) {
        p(cells)
      } else {
        Left(s"Input size [${cells.length}] does not match parser expected size [$s]")
      }
    }
    def size = s
    def to(t: T): String = stringifier(t)
    val header: List[String] = toHeader
  }

  /**
    * Recursively create a parser for an HList.
    *
    * Base Case: given an empty sequence of cells, produce an HNil
    */
  implicit val hnilParser: CsvParser[HNil] = instance[HNil](0)(s => Right((HNil)), { _ => "" })

  /**
    * Recursively create a parser for an HList.
    *
    * Inductive Step: given a sequence of cells, parse the front cells into `Head` and
    * the sequence of remaining cells into `Tail`.
    * `Head` is not limited to one cell; `Head` may be a class requiring multiple cells.
    */
  implicit def hconsParser[K <: Symbol, V, Tail <: HList]
  (implicit hParser: Lazy[CsvParser[V]],
   key: Witness.Aux[K],
   tParser: CsvParser[Tail]): CsvParser[FieldType[K,V] :: Tail] = {
    instance(hParser.value.size + tParser.size)({ cells =>
      val (headCells, tailCells) = cells.splitAt(hParser.value.size)
      for {
        head <- hParser.value.parse(headCells).right
        tail <- tParser.parse(tailCells).right
      } yield field[K](head) :: tail
    },
    { case (h :: t) => List(hParser.value.to(h), tParser.to(t)).mkString(",") },
    { if (hParser.value.size == 1) key.value.name +: tParser.header
      else hParser.value.header ++ tParser.header
    })
  }

  /**
    * Create a `CsvParser` for a case class of type `Case`. The parser requires
    * that we can safely convert an HList `Repr` into a `Case`
    * @param gen The Generic allowing to convert from a `Repr` HList to a `Case` case class
    * @param reprParser The `CsvParser` that can parse a CSV line into an HList of type `Repr`
    */
  implicit def caseClassParser[Case, Repr <: HList]
  (implicit
   gen: LabelledGeneric.Aux[Case, Repr],
   reprParser: Lazy[CsvParser[Repr]]): CsvParser[Case] = {
    instance(reprParser.value.size) ({ cells =>
      reprParser.value.parse(cells).right.map { parsed =>
        (gen.from(parsed))
      }
    },
    { c => reprParser.value.to(gen.to(c)).init },
    { reprParser.value.header })
  }
}