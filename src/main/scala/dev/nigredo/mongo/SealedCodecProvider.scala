package dev.nigredo.mongo

import org.bson.codecs.configuration.{CodecConfigurationException, CodecProvider, CodecRegistry}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocumentReader, BsonDocumentWriter, BsonReader, BsonWriter}
import org.mongodb.scala.bson.{BsonDocument, BsonValue}

import scala.collection.JavaConverters._

/**
  * Codec provider for sealed family
  */
trait SealedCodecProvider[T] extends CodecProvider {

  final val TypeKey = "_type"
  val codecProviders: Seq[CodecProvider]
  val classMapping: Map[String, Class[_]]

  private def findCodec[C](clazz: Class[C], registry: CodecRegistry): Codec[C] =
    codecProviders.collectFirst {
      case x if x.get(clazz, registry) != null => x.get(clazz, registry)
    }.getOrElse(throw new CodecConfigurationException(s"Can't find a codec for class $clazz"))

  class SealedCodec[C](clazz: Class[C], val registry: CodecRegistry) extends Codec[C] {

    override def encode(writer: BsonWriter, value: C, encoderContext: EncoderContext): Unit = {
      val (valueClassName, _) = classMapping.find {
        case (_, v) => v.isAssignableFrom(clazz)
      }.getOrElse(throw new CodecConfigurationException(s"Can't find type for class $clazz"))
      writer.writeStartDocument()
      writer.writeString(TypeKey, valueClassName)
      val childDoc = BsonDocument()
      findCodec(clazz, registry).encode(new BsonDocumentWriter(childDoc), value, encoderContext.getChildContext)
      childDoc.asScala.foreach { case (k, v) =>
        writer.writeName(k)
        encoderContext.encodeWithChildContext(registry.get(v.getClass).asInstanceOf[Codec[BsonValue]], writer, v)
      }

      writer.writeEndDocument()
    }

    override def getEncoderClass: Class[C] = clazz

    override def decode(reader: BsonReader, decoderContext: DecoderContext): C = {
      val bsonDoc = registry.get(classOf[BsonDocument]).decode(reader, decoderContext)
      val valueClassName = bsonDoc.remove(TypeKey).asString().getValue
      val (_, className) = classMapping.find(x => x._1 == valueClassName)
        .getOrElse(throw new CodecConfigurationException(s"Can't find class for type $valueClassName"))
      findCodec(className, registry).asInstanceOf[Codec[C]].decode(new BsonDocumentReader(bsonDoc), decoderContext)
    }
  }

}