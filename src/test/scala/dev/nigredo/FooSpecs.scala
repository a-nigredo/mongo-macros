package dev.nigredo


import org.mongodb.scala.bson.{BsonDocument, ObjectId}
import org.specs2.mutable.Specification

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class FooSpecs extends Specification {
 "Foo" should {
   "bar" in {

     import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromRegistries}
     import org.mongodb.scala._
     import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
     val barRegistry = fromRegistries(fromCodecs(Codec.gen[Bar](DEFAULT_CODEC_REGISTRY)))
     val codecRegistry = fromRegistries(
       fromCodecs(Codec.gen[Foo](barRegistry)),
       DEFAULT_CODEC_REGISTRY)
     val database = MongoClient().getDatabase("test").withCodecRegistry(codecRegistry)
     val collection = database.getCollection[Foo]("foo")
     Await.result(collection.deleteMany(BsonDocument()).toFuture(), Duration.Inf)
     Await.result(collection.insertOne(Foo(new ObjectId(), "test", 12,42L, 43.400, true, "str".getBytes, 1.toByte, Bar("bla"), Seq("str", "str2"))).toFuture(), Duration.Inf)
     Await.result(collection.insertOne(Foo(new ObjectId(), "test", 12, 42L, 43.300, false, "str".getBytes, 1.toByte, Bar("bla"), Seq.empty, Some("str"))).toFuture(), Duration.Inf)
     val res = Await.result(collection.find[Foo]().toFuture(), Duration.Inf)
     println(res)
     ko
   }
 }
}
