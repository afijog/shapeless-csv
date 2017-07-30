package es.massimple.csv

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.OptionValues
import scala.util.Try

trait Invalid
case class Test(s: String, i: Int, d: Double)
case class OptionHolder(id: Int, o1: Option[Test])

class CsvParserSpec extends WordSpec with Matchers with OptionValues with TestFixtures {
  "A CsvParser" should {
    "parse a single CSV line into a case class" in {
      val parsedCaseClasses = CsvParser[Test].parse("hello world,12345,3.14")
      parsedCaseClasses.size should equal (1)
      val parsedCaseClass = parsedCaseClasses(0)
      parsedCaseClass.right.get.s should equal ("hello world")
      parsedCaseClass.right.get.i should equal (12345)
      parsedCaseClass.right.get.d should equal (3.14)
    }

    "fail to parse case class when given too few arguments" in {
      val parsedCaseClasses = CsvParser[Test].parse("hello world,12345")
      parsedCaseClasses.size should equal (1)
      val parsedCaseClass = parsedCaseClasses(0)
      parsedCaseClass should equal (Left(s"Input size [2] does not match parser expected size [3]"))
    }

    "fail to parse case class when given too many arguments" in {
      val parsedCaseClasses = CsvParser[Test].parse("hello world,12345,3.14,54321")
      parsedCaseClasses.size should equal (1)
      val parsedCaseClass = parsedCaseClasses(0)
      parsedCaseClass should equal (Left(s"Input size [4] does not match parser expected size [3]"))
    }

    "fail to parse case class when given incorrect types" in {
      val parsedCaseClasses = CsvParser[Test].parse("hello,12345,world")
      parsedCaseClasses.size should equal (1)
      val parsedCaseClass = parsedCaseClasses(0)
      parsedCaseClass shouldBe a [Left[_, _]]
    }

    "fail to compile when given invalid type" in {
      """val parsedCaseClass = CsvParser[Invalid].parse("hello world")""" shouldNot compile
    }

    "parse multiple lines of Bytes" in {
      val parsedBytes = CsvParser[Byte].parse("1\n2\n3")
      parsedBytes should contain allOf(Right(1), Right(2), Right(3))
    }

    "fail to parse invalid data into Bytes" in {
      val parsedBytes = CsvParser[Byte].parse("a\n128\n1,2")
      parsedBytes should contain allOf (Left(s"Cannot parse [a] to Byte"),
        Left(s"Cannot parse [128] to Byte"),
        Left(s"Cannot parse [1,2] to Byte"))
    }

    "parse multiple lines of Shorts" in {
      val parsedShorts = CsvParser[Short].parse("1\n2\n3")
      parsedShorts should contain allOf (Right(1), Right(2), Right(3))
    }

    "fail to parse invalid data into Shorts" in {
      val parsedShorts = CsvParser[Short].parse("a\n32768\n1,2")
      parsedShorts should contain allOf (Left(s"Cannot parse [a] to Short"),
        Left(s"Cannot parse [32768] to Short"),
        Left(s"Cannot parse [1,2] to Short"))
    }

    "parse multiple lines of Ints" in {
      val parsedInts = CsvParser[Int].parse("1\n2\n3")
      parsedInts should contain allOf (Right(1), Right(2), Right(3))
    }

    "fail to parse invalid data into Ints" in {
      val parsedInts = CsvParser[Int].parse("a\n2147483648\n1,2")
      parsedInts should contain allOf (Left(s"Cannot parse [a] to Int"),
        Left(s"Cannot parse [2147483648] to Int"),
        Left(s"Cannot parse [1,2] to Int"))
    }

    "parse multiple lines of Longs" in {
      val parsedLongs = CsvParser[Long].parse("1\n2\n300000000000000")
      parsedLongs should contain allOf (Right(1L), Right(2L), Right(300000000000000L))
    }

    "fail to parse invalid data into Longs" in {
      val parsedLongs = CsvParser[Long].parse("a\n9223372036854775808\n1,2")
      parsedLongs should contain allOf (Left(s"Cannot parse [a] to Long"),
        Left(s"Cannot parse [9223372036854775808] to Long"),
        Left(s"Cannot parse [1,2] to Long"))
    }

    "parse multiple lines of Strings" in {
      val parsedStrings = CsvParser[String].parse("hello\nworld\nmoon")
      parsedStrings should contain allOf (Right("hello"), Right("world"), Right("moon"))
    }

    "parse multiple lines of Chars" in {
      val parsedChars = CsvParser[Char].parse("a\nb\nc")
      parsedChars should contain allOf(Right('a'), Right('b'), Right('c'))
    }

    "fail to parse invalid data into Chars" in {
      val parsedChars = CsvParser[Char].parse("abc\n123\na,b")
      parsedChars should contain allOf (Left(s"Cannot parse [abc] to Char"),
        Left(s"Cannot parse [123] to Char"),
        Left(s"Cannot parse [a,b] to Char"))
    }

    "parse multiple lines of Floats" in {
      val parsedFloats = CsvParser[Float].parse("3.14\n2.22\n0.11")
      parsedFloats should contain allOf (Right(3.14f), Right(2.22f), Right(0.11f))
    }

    "fail to parse invalid data into Floats" in {
      val parsedFloats = CsvParser[Float].parse("a\nxyz\n1,2")
      parsedFloats should contain allOf (Left(s"Cannot parse [a] to Float"),
        Left(s"Cannot parse [xyz] to Float"),
        Left(s"Cannot parse [1,2] to Float"))
    }

    "parse multiple lines of Doubles" in {
      val parsedDoubles = CsvParser[Double].parse("3.14\n2.22\n0.11")
      parsedDoubles should contain allOf (Right(3.14), Right(2.22), Right(0.11))
    }

    "fail to parse invalid data into Doubles" in {
      val parsedDoubles = CsvParser[Double].parse("a\nxyz\n1,2")
      parsedDoubles should contain allOf (Left(s"Cannot parse [a] to Double"),
        Left(s"Cannot parse [xyz] to Double"),
        Left(s"Cannot parse [1,2] to Double"))
    }

    "parse multiple lines of Booleans" in {
      val parsedBooleans = CsvParser[Boolean].parse("true\nfalse")
      parsedBooleans should contain allOf (Right(true), Right(false))
    }

    "parse multiple lines of Option" in {
      val parsedBooleans = CsvParser[Option[Boolean]].parse("true\nfalse")
      parsedBooleans should contain allOf (Right(Some(true)), Right(Some(false)))
    }

    "fail to parse invalid data into Booleans" in {
      val parsedBooleans = CsvParser[Boolean].parse("a\n1\ntrue,false")
      parsedBooleans should contain allOf (Left(s"Cannot parse [a] to Boolean"),
        Left(s"Cannot parse [1] to Boolean"),
        Left(s"Cannot parse [true,false] to Boolean"))
    }

    "parse multiple lines into case classes" in {
      case class Test(a: Int, b: Double)
      val parsedTests = CsvParser[Test].parse("3,0.14\n1,0.45")
      parsedTests should contain allOf (Right(Test(3, 0.14)), Right(Test(1, 0.45)))
    }

    "partially parse a CSV with different types of lines" in {
      val parsedCsv = CsvParser[Int].parse("1\nhello\n3.14")
      parsedCsv should contain (Right(1))
      parsedCsv should contain (Left(s"Cannot parse [hello] to Int"))
      parsedCsv should contain (Left(s"Cannot parse [3.14] to Int"))
    }

    "parse if given an implicit" in {
      implicit val longParser = CsvParser.headInstance(s => Try(s.toLong + 3).toOption)(s => "Could not parse [$s] to Long")(_.toString)
      val parsedLongs = CsvParser[Long].parse("1234567890")
      parsedLongs.size should equal (1)
      val parsedLong = parsedLongs(0)
      parsedLong.right.get should equal (1234567890 + 3)
    }

    "parse nested case classes" in {
      case class Holder(t: Test)
      val parsedHolders = CsvParser[Holder].parse("hello world,54321,3.14")
      parsedHolders.size should equal (1)
      val parsedHolder = parsedHolders(0)
      parsedHolder should equal (Right(Holder(Test("hello world", 54321, 3.14))))
    }

    "parse padded nested case class with header" in {
      case class Holder(header: Int, t: Test)
      val parsedHolders = CsvParser[Holder].parse("3,hello world,54321,3.14")
      parsedHolders.size should equal (1)
      val parsedHolder = parsedHolders(0)
      parsedHolder should equal (Right(Holder(3, Test("hello world", 54321, 3.14))))
    }

    "parse padded nested case class with footer" in {
      case class Holder(t: Test, footer: Int)
      val parsedHolders = CsvParser[Holder].parse("hello world,54321,3.14,3")
      parsedHolders.size should equal (1)
      val parsedHolder = parsedHolders(0)
      parsedHolder should equal (Right(Holder(Test("hello world", 54321, 3.14), 3)))
    }

    "parse padded nested case class with header/footer" in {
      case class Holder(header: Int, t: Test, footer: Int)
      val parsedHolders = CsvParser[Holder].parse("4,hello world,54321,3.14,3")
      parsedHolders.size should equal (1)
      val parsedHolder = parsedHolders(0)
      parsedHolder should equal (Right(Holder(4, Test("hello world", 54321, 3.14), 3)))
    }

    "ignore header if given hasHeader = true" in {
      case class Test(a: String, b: Int)
      val parsedTests = CsvParser[Test].parse("name,age\nwaldo,28", hasHeader = true)
      parsedTests should contain only (Right(Test("waldo", 28)))
    }

    "ignore headers if explicitly told to" in {
      case class Test(a: String, b: Int)
      val parsedTests = CsvParser[Test].parseSkipHeader("name,age\nwaldo,28")
      parsedTests should contain only (Right(Test("waldo", 28)))
    }

    "parse from String with custom case classes" in {
      val expected = List(test3)
      val parsedTests = CsvParser[Test3].parse(test3AsText)
      val actual = parsedTests flatMap { _.right.toOption }
      expected should equal (actual)
    }

    "convert to CSV with custom case classes" in {
      val expected = test3AsText
      val actual = CsvParser[Test3].to(test3)
      expected should equal (actual)
    }

    "convert to header with custom case classes" in {
      val expected = List("id", "name",
        "o1",
        "sc11", "sc12", "sc21", "sc22",
        "o2",
        "sc11", "sc12", "sc21", "sc22",
        "a", "b", "c")

      val actual = CsvParser[Test3].header
      expected should be (actual)
    }

    "parse from String with custom case classes (II)" in {
      val expected = List(calculator)
      val parsedTests = CsvParser[Calculator].parse(calculatorAsText)
      val actual = parsedTests flatMap { _.right.toOption }
      expected should be (actual)
    }

    "convert to CSV with custom case classes (II)" in {
      val expected = calculatorAsText
      val actual = CsvParser[Calculator].to(calculator)
      expected should be (actual)
    }

    "convert to header with custom case classes (II)" in {
      val expected = List("brand","model","mem","date")
      val actual = CsvParser[Calculator].header
      expected should be (actual)
    }

    "parse multiple lines of DateTime" in {
      val parsedDates = CsvParser[DateTime].parse("2017-07-11\n2017-07-12")
      parsedDates should contain allOf (
        Right(new DateTime("2017-07-11")),
        Right(new DateTime("2017-07-12")))
    }

    "parse multiple lines of DateTime with custom date format" in {
      implicit val dtfDate: DateTimeFormatter = DateTimeFormat.forPattern("YYYY-MM")
      implicit val jodaDateParser = CsvParser.jodaDateParserBuilder

      val parsedDates = CsvParser[DateTime].parse("2017-07\n2017-08")
      parsedDates should contain allOf (
        Right(new DateTime("2017-07")),
        Right(new DateTime("2017-08")))
    }

    "parse multiple lines of DateTime with custom date format in case class" in {
      case class DateHolder(date: DateTime)

      implicit val dtfDate: DateTimeFormatter = DateTimeFormat.forPattern("YYYY-MM")
      implicit val jodaDateParser = CsvParser.jodaDateParserBuilder

      val parsedDates = CsvParser[DateHolder].parse("2017-07\n2017-08")
      parsedDates should contain allOf (
        Right(DateHolder(new DateTime("2017-07"))),
        Right(DateHolder(new DateTime("2017-08"))))
    }
  }

  "parse line of Option[CaseClass] is None" in {
    val parsedTests = CsvParser[OptionHolder].parse("1,,,")
    parsedTests should contain only (Right(OptionHolder(1, None)))
  }

  "parse line of Option[CaseClass] is Some(_)" in {
    val parsedTests = CsvParser[OptionHolder].parse("1,string,1,2.0")
    parsedTests should contain only (Right(OptionHolder(1, Some(Test("string", 1, 2.0)))))
  }

  "parse line of Option[CaseClass] with error" in {
    val parsedCsv = CsvParser[OptionHolder].parse("1,string,1a,2")
    parsedCsv should contain (Left(s"Cannot parse [1a] to Int"))
  }

  "convert to CSV when Option[CaseClass] is None" in {
    val o = OptionHolder(1, None)
    val expected = "1,,,"
    val actual = CsvParser[OptionHolder].to(o)
    expected shouldEqual actual
  }
}