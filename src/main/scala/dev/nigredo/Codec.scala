package dev.nigredo


import org.bson.codecs.configuration.CodecRegistry

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Codec {
  def gen[A](registry: CodecRegistry): org.bson.codecs.Codec[A] = macro CodecImpl.impl[A]
}

object CodecImpl {
  def impl[A: c.WeakTypeTag](c: blackbox.Context)(registry: c.Expr[CodecRegistry]): c.Expr[org.bson.codecs.Codec[A]] = {
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

      def typeFieldName(base: String) = s"_${base}_type"

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
          if (weakTypeOf[org.mongodb.scala.bson.ObjectId].typeSymbol == typeSymbol)
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
          } else if (typeSymbol.isSealed) {
            q"""
                ${buildPath(path.:+(sm.name.toTermName), q"")} match { case ..${
              typeSymbol.knownDirectSubclasses.map(x => {
                val name = TermName(c.freshName())
                val fieldName = sm.name.encodedName.toString
                cq"""$name: $x =>
                    writer.writeString(${typeFieldName(fieldName)}, ${x.name.encodedName.toString})
                    writer.writeName($fieldName)
                    ${
                  if (x.isModuleClass) q"writer.writeString(${x.name.encodedName.toString})"
                  else q"$registry.get(classOf[$x]).encode(writer, ${buildPath(List(name))}, encoderContext)"
                }
                    """
              })
            }}"""
          }
          else if (typeSymbol.isModuleClass) {
            val fieldName = sm.name.encodedName.toString
            q"""
                  writer.writeString(${typeFieldName(fieldName)}, ${typeSymbol.name.encodedName.toString})
                  writer.writeName($fieldName)
                  writer.writeString(${typeSymbol.name.encodedName.toString})
               """
          }
          else if (typeSymbol.isCaseClass) {
            q"""
               writer.writeName(${sm.name.encodedName.toString})
               $registry.get(classOf[$typeSymbol]).encode(writer, ${buildPath(path.:+(sm.name.toTermName))}, encoderContext)
             """
          } else q""
        }
       """
      }.filter(_.nonEmpty)
    }

    def readerImpl(sym: ClassSymbol, path: List[TermName] = Nil): c.universe.Tree = {
      val code =
        q"""
          val caseClassAsMap = Map[String, Class[_]](..${
          getPrimaryCtorParams(sym)
            .map(x => q"${x.name.encodedName.toString} -> classOf[${x.typeSignature.typeSymbol}]")
        })
          val name = reader.readName()
          caseClassAsMap.get(name).fold(reader.skipValue()){ x =>
            if(x.isAssignableFrom(classOf[${c.universe.definitions.StringClass}])) dataAsMap.+=(name -> reader.readString())
            else if(x.isAssignableFrom(classOf[${c.universe.definitions.IntClass}])) dataAsMap.+=(name -> reader.readInt32())
            else reader.skipValue()
          }"""

      q"""
         reader.readStartDocument()
         while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
          ..$code
         }
         reader.readEndDocument()
       """
    }

    val code =
      q"""
       new org.bson.codecs.Codec[$ts] {
            override def decode(reader: org.bson.BsonReader, decoderContext: org.bson.codecs.DecoderContext): $ts = {
              val dataAsMap = scala.collection.mutable.Map.empty[String, Any]
              ${readerImpl(ts.typeSymbol.asClass)}
              new $ts(..${
        getPrimaryCtorParams(ts.typeSymbol.asClass).map(x =>
          q"""${x.name.toTermName} = dataAsMap.get(${x.name.encodedName.toString})
             .map(_.asInstanceOf[${x.typeSignature.typeSymbol.name.toTypeName}])
             .getOrElse(throw new IllegalStateException("Could not find field " + ${ts.typeSymbol.name.encodedName.toString} + "#" + ${x.name.encodedName.toString} + " in bson"))""")
      })}

          override def encode(writer: org.bson.BsonWriter, value: $ts, encoderContext: org.bson.codecs.EncoderContext): Unit =
              ${q"..${writerImpl(ts.typeSymbol.asClass, List(TermName("value"))).+:(q"writer.writeStartDocument()").:+(q"writer.writeEndDocument()")}"}

          override def getEncoderClass: Class[$ts] = classOf[$ts]
          }
     """
    println(showCode(code))
    c.Expr[org.bson.codecs.Codec[A]](code)
  }
}
