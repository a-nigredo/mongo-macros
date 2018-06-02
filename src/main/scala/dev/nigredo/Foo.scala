package dev.nigredo

import org.mongodb.scala.bson.ObjectId

case class Foo(id: ObjectId,
               value1: String, value2: Int, value3: Long, value4: Double, value5: Boolean, bb: Seq[Byte], byte: Byte, bar: Bar, list: Seq[String],
               opt: Option[String] = None)

case class Bar(v: String)

case class SimpleFoo(adt: Adt = Adt3, adt2: Adt3.type = Adt3)

sealed trait Adt

case class Adt1(v: String) extends Adt

case class Adt2(v: Int) extends Adt

case object Adt3 extends Adt
