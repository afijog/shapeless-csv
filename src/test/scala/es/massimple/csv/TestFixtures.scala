package es.massimple.csv

import org.joda.time.DateTime

trait TestFixtures {
  case class Calculator(brand: String, model: String, mem: Int, date: Option[DateTime])
  val calculator = Calculator("HP", "20b", 128, Some(new DateTime("2017-07-11")))
  val calculatorAsText = "HP,20b,128,2017-07-11"



  case class Test1(a: String, b: Int, c: Int)
  case class Test2(sc11: Option[Int], sc12: Option[Int], sc21: Option[Int], sc22: Option[Int])
  case class Test3(id: Option[Int], name:String,
                   o1: Option[Int],
                   t21: Test2,
                   o2: Option[Int],
                   t22: Test2,
                   t1: Test1)

  val test1  = Test1("n",3,3)
  val test21 = Test2(None, None, Some(13), Some(14))
  val test22 = Test2(None, Some(2), None, Some(4))
  val test3 = Test3(Some(12), "name", None, test21, Some(55), test22, test1)

  val test3AsText = "12,name,,,,13,14,55,,2,,4,n,3,3"

}