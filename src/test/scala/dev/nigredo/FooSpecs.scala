package dev.nigredo


import org.bson.{BsonReader, BsonWriter}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
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

//     val barRegistry = fromRegistries(fromCodecs(Codec.gen[Bar](DEFAULT_CODEC_REGISTRY)))
     val adt1Reg = fromRegistries(fromCodecs(Codec.gen[Adt1](DEFAULT_CODEC_REGISTRY)))
     val adt2Reg = fromRegistries(fromCodecs(Codec.gen[Adt2](DEFAULT_CODEC_REGISTRY)))
     val codecRegistry = fromRegistries(fromCodecs(Codec.gen[SimpleFoo](fromRegistries(adt1Reg, adt2Reg))), DEFAULT_CODEC_REGISTRY)
     val database = MongoClient().getDatabase("test").withCodecRegistry(codecRegistry)
     val collection = database.getCollection[SimpleFoo]("foo")
     Await.result(collection.deleteMany(BsonDocument()).toFuture(), Duration.Inf)
//     Await.result(collection.insertOne(Foo(new ObjectId(), "test", 12,42L, 43.400, true, "str".getBytes, 1.toByte, Bar("bla"), Seq("str", "str2"))).toFuture(), Duration.Inf)
//     Await.result(collection.insertOne(Foo(new ObjectId(), "test", 12, 42L, 43.300, false, "str".getBytes, 1.toByte, Bar("bla"), Seq.empty, Some("str"))).toFuture(), Duration.Inf)
     Await.result(collection.insertOne(SimpleFoo()).toFuture(), Duration.Inf)
     val res = Await.result(collection.find[SimpleFoo]().toFuture(), Duration.Inf)
     println(res)
     ko
   }
 }
}
