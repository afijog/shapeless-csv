Shapeless CSV
=========================

A boilerplate-free CSV parser using shapeless with Option and automatic header derivation for Scala case classes.

Original idea and code from Travis Brown's http://meta.plasm.us/posts/2015/11/08/type-classes-and-generic-derivation/ and Jake Greene's https://github.com/JakeGreene/shapeless-csv

Usage
-----

Able to parse CSV strings into case classes without the need for boilerplate. Supports nested case classes with Option monad.
Also has the ability to build a header for the case class using all the field names and nested classes.

```scala
import es.massimple.csv._

case class Test1(a: String, b: Int, c: Int)
case class Test2(sc11: Option[Int], sc12: Option[Int], sc21: Option[Int], sc22: Option[Int])
case class Test3(id: Option[Int], name:String,
               o1: Option[Int],
               t21: Test2,
               o2: Option[Int],
               t22: Test2,
               t1: Test1)

val csv = "12,name,,,,13,14,55,,2,,4,n,3,3"
val test3 = CsvParser[Test3].parse(csv)
/*
 * List(Right(
 *   Test3(Some(12),name,
 *       None,
 *      Test2(None,None,Some(13),Some(14)),
 *      Some(55),
 *      Test2(None,Some(2),None,Some(4)),
 *      Test1(n,3,3))))
 */
println(test3)

val runtimeCheck = CsvParser[Test1].parse("Test1,26,true")
// List(Left(Cannot parse [true] to Int))
println(runtimeCheck)

implicit val dtfDate: DateTimeFormatter = DateTimeFormat.forPattern("YYYY-MM") // Defaults to "YYYY-MM-dd"
implicit val jodaDateParser = CsvParser.jodaDateParserBuilder

val parsedDates = CsvParser[DateTime].parse("2017-07\n2017-08")
// List(Right(2017-07-01T00:00:00.000+02:00), Right(2017-08-01T00:00:00.000+02:00))
println(parsedDates)


trait Invalid
/* <console>:14: error: could not find implicit value for parameter parser: es.massimple.csv.CsvParser[Invalid]
 *     val compiletimeCheck = CsvParser[Invalid].parse("Invalid test data")
 */
val compiletimeCheck = CsvParser[Invalid].parse("Invalid test data")
```
