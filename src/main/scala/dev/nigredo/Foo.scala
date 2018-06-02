package dev.nigredo

import org.mongodb.scala.bson.ObjectId

case class Foo(id: ObjectId,
                value1: String, value2: Int, value3: Long, value4: Double, value5: Boolean, bb: Seq[Byte], byte: Byte, bar: Bar, list: Seq[String],
               opt: Option[String] = None)

case class Bar(v: String)
