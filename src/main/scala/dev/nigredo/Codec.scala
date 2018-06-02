package dev.nigredo

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistry

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Codec {
  def gen[A](registry: CodecRegistry): Codec[A] = macro CodecImpl.impl[A]
}

object CodecImpl {
  def impl[A: c.WeakTypeTag](c: blackbox.Context)(registry: c.Expr[CodecRegistry]): c.Expr[Codec[A]] = {
    import c.universe._
    val ts = weakTypeOf[A]

    @tailrec
    def buildPath(path: Seq[TermName], value: c.universe.Tree = q""): c.universe.Tree =
      if (path.isEmpty) value
      else {
        val field = path.head
        if (value.isEmpty) buildPath(path.tail, q"$field")
        else buildPath(path.tail, q"$value.$field")
      }

    def getPrimaryCtorParams(sym: ClassSymbol): Seq[c.universe.Symbol] = {
      if (sym.isPrimitive || c.universe.definitions.StringClass == sym) Seq(sym)
      else
        sym.typeSignature.decls.flatMap {
          case m: MethodSymbol if m.isPrimaryConstructor => m.typeSignature match {
            case method: MethodType if method.params.nonEmpty => m.paramLists
          }
          case _ => Seq.empty
        }.flatten.toSeq
    }

    def writerImpl(sym: ClassSymbol, path: List[TermName] = Nil, writeFieldName: Boolean = true): Seq[c.universe.Tree] = {
      getPrimaryCtorParams(sym).map { sm =>
        val typeSymbol = sm.typeSignature.typeSymbol.asClass
        val p = if (sm.isType) path else path.:+(sm.name.toTermName)
        val fieldName = sm.name.encodedName.toString

        def write(methodName: String) = {
          val methodNameTermName = TermName(methodName)
          if (writeFieldName)
            q"""writer.$methodNameTermName($fieldName, ${buildPath(p)})"""
          else
            q"""writer.$methodNameTermName(${buildPath(p)})"""
        }

        q"""
         ${
          if(weakTypeOf[org.mongodb.scala.bson.ObjectId].typeSymbol == typeSymbol)
            write("writeObjectId")
          else if (c.universe.definitions.StringClass == typeSymbol || c.universe.definitions.CharClass == typeSymbol)
            write("writeString")
          else if (c.universe.definitions.IntClass == typeSymbol ||
            c.universe.definitions.ShortClass == typeSymbol ||
            c.universe.definitions.ByteClass == typeSymbol)
            write("writeInt32")
          else if (c.universe.definitions.LongClass == typeSymbol) write("writeInt64")
          else if (c.universe.definitions.DoubleClass == typeSymbol || c.universe.definitions.FloatClass == typeSymbol)
            write("writeDouble")
          else if (c.universe.definitions.BooleanClass == typeSymbol) write("writeBoolean")
          else if (typeSymbol.toType.<:<(typeOf[Seq[Any]])) {
            val termName = TermName(c.freshName())
            val typeArg = sm.typeSignature.typeArgs.head.typeSymbol.asClass
            if (c.universe.definitions.ByteClass == typeArg) {
              q"""
               writer.writeBinaryData($fieldName, new org.bson.BsonBinary(${buildPath(path.:+(sm.name.toTermName))}.toArray))
             """
            } else {
              q"""
               writer.writeStartArray(${sm.name.encodedName.toString})
               ${buildPath(path.:+(sm.name.toTermName))}.foreach(($termName: $typeArg) => {
               ..${writerImpl(typeArg, List(termName), false)}
               })
               writer.writeEndArray()
             """
            }
          } else if (typeSymbol.toType.<:<(typeOf[Option[Any]])) {
            val termName = TermName(c.freshName())
            val typeArg = sm.typeSignature.typeArgs.head.typeSymbol.asClass
            q"""
               writer.writeName(${sm.name.encodedName.toString})
               ${buildPath(path.:+(sm.name.toTermName))} match {
                  case None => writer.writeNull()
                  case Some($termName) => ..${writerImpl(typeArg, List(termName), false)}
               }
             """
          } else if (sym.isCaseClass) {
            q"""
               writer.writeName(${sm.name.encodedName.toString})
               $registry.get[$typeSymbol](classOf[$typeSymbol]).encode(writer, ${buildPath(path.:+(sm.name.toTermName))}, encoderContext)
             """
          } else q""
        }
       """
      }.filter(_.nonEmpty)
    }

    def readerImpl(sym: ClassSymbol, path: List[TermName] = Nil): Seq[c.universe.Tree] = {
      getPrimaryCtorParams(sym).map { sm =>
        val typeSymbol = sm.typeSignature.typeSymbol.asClass
        q"""
           val data = mutable.Map.empty[String, Any]
           while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
              val cbt = reader.getCurrentBsonType
              val name = reader.readName()
              if(cbt == BsonType.STRING) {
                 map.+=(name -> reader.readString())
              } else if(cbt == BsonType.DOUBLE) {
                 map.+=(name -> reader.readDouble())
              } else if(cbt == BsonType.INT32) {
                 map.+=(name -> reader.readInt32())
              } else {
                 reader.skipValue()
              }
           }
         """
      }.filter(_.nonEmpty).+:(q"reader.readStartDocument()").:+(q"writer.readEndDocument()")
    }

    val code =
      q"""
       new org.bson.codecs.Codec[$ts] {
            override def decode(reader: org.bson.BsonReader, decoderContext: org.bson.codecs.DecoderContext): $ts = {
              ???
            }

          override def encode(writer: org.bson.BsonWriter, value: $ts, encoderContext: org.bson.codecs.EncoderContext): Unit =
              ${q"..${writerImpl(ts.typeSymbol.asClass, List(TermName("value"))).+:(q"writer.writeStartDocument()").:+(q"writer.writeEndDocument()")}"}

          override def getEncoderClass: Class[$ts] = classOf[$ts]
          }
     """
    c.Expr[Codec[A]](code)
  }
}

//val map = scala.collection.mutable.Map.empty[String, Any]
//reader.readStartDocument()
//while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
//val cbt = reader.getCurrentBsonType
//val name = reader.readName()
//if(cbt == BsonType.STRING) {
//map.+=(name -> reader.readString())
//} else if(cbt == BsonType.DOUBLE) {
//map.+=(name -> reader.readDouble())
//} else if(cbt == BsonType.INT32) {
//map.+=(name -> reader.readInt32())
//} else {
//reader.skipValue()
//}
//}
//reader.readEndDocument()
//println(map)
