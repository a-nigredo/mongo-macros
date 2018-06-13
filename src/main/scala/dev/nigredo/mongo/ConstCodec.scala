package dev.nigredo.mongo

import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonWriter}

/**
  * Const codec always returns defined value. It is used for case object representation in bson
  */
abstract class ConstCodec[T](value: T) extends Codec[T] {

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit = ()

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T = value
}