package dev.nigredo.mongo

import org.bson.codecs.configuration.CodecRegistry

import scala.reflect.macros.blackbox
import language.experimental.macros

object Macros {

  def createSealedCodecProvider[A]: org.bson.codecs.configuration.CodecProvider = macro MacrosImpl.createSealedCodecProviderImpl[A]

  def createConstCodec[A](value: A): org.bson.codecs.Codec[A] = macro MacrosImpl.createConstCodecImpl[A]

  def createConstCodecProvider[A](value: A): org.bson.codecs.configuration.CodecProvider = macro MacrosImpl.createConstCodecProviderImpl[A]

  def createCodecProvider[A]: org.bson.codecs.configuration.CodecProvider = macro MacrosImpl.createCodecProviderImpl[A]

  def createCodec[A](registry: CodecRegistry): org.bson.codecs.Codec[A] = macro MacrosImpl.createCodecImpl[A]
}

object MacrosImpl {

  def createCodecProviderImpl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[org.bson.codecs.configuration.CodecProvider] = {

    import c.universe._
    val ts = weakTypeOf[A].typeSymbol.asClass

    if (!ts.asClass.isCaseClass) c.abort(c.enclosingPosition, s"Type ${ts.fullName} is not a case class. Codec provider can be derived from case class only")
    c.Expr[org.bson.codecs.configuration.CodecProvider](
      q"""
         new CodecProvider {
              override def get[A](clazz: Class[A], registry: CodecRegistry): Codec[A] = {
                if (classOf[$ts].isAssignableFrom(clazz)) Macros.createCodec[$ts](registry).asInstanceOf[Codec[A]]
                else null
              }
         }
       """)
  }

  def createCodecImpl[A: c.WeakTypeTag](c: blackbox.Context)(registry: c.Expr[CodecRegistry]): c.Expr[org.bson.codecs.Codec[A]] = {

    import c.universe._
    val ts = weakTypeOf[A].typeSymbol.asClass

    if (!ts.asClass.isCaseClass) c.abort(c.enclosingPosition, s"Type ${ts.fullName} is not a case class. Codec can be derived from case class only")

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

    def buildCls(`type`: Type) = if (`type`.typeArgs.nonEmpty) tq"${`type`}" else tq"${`type`.typeSymbol}"

    def toJavaPrimitive(sym: Symbol) = {
      Map(
        c.universe.definitions.BooleanClass -> typeOf[java.lang.Boolean],
        c.universe.definitions.ByteClass -> typeOf[java.lang.Byte],
        c.universe.definitions.CharClass -> typeOf[java.lang.Character],
        c.universe.definitions.DoubleClass -> typeOf[java.lang.Double],
        c.universe.definitions.FloatClass -> typeOf[java.lang.Float],
        c.universe.definitions.IntClass -> typeOf[java.lang.Integer],
        c.universe.definitions.LongClass -> typeOf[java.lang.Long],
        c.universe.definitions.ShortClass -> typeOf[java.lang.Short]
      ).find(_._1 == sym).map(_._2)
        .getOrElse(c.abort(c.enclosingPosition, s"Could not find java primitive for ${sym.fullName}"))
    }

    val codecName = TypeName(s"${ts.name.encodedName.toString}Codec")
    val codec =
      q"""
         class $codecName(registry: CodecRegistry) extends Codec[$ts] {

           override def getEncoderClass: Class[$ts] = classOf[$ts]

           override def encode(writer: BsonWriter, value: $ts, encoderContext: EncoderContext): Unit = {
            writer.writeStartDocument()
            ..${
        getPrimaryCtorParams(ts).map { sym =>
          val cls = sym.typeSignature.typeSymbol.asClass
          q"""
              val currentValue = value.${sym.name.toTermName}
              writer.writeName(${sym.name.encodedName.toString})
              ${
            if (cls.toType.<:<(typeOf[Option[Any]])) {
              val termName = TermName(c.freshName())
              val typeArg = sym.typeSignature.typeArgs.head.typeSymbol
              q"""
               currentValue match {
                  case None => writer.writeNull()
                  case Some($termName) =>
                  registry.get(classOf[${if (typeArg.asClass.isPrimitive) tq"${toJavaPrimitive(typeArg)}" else buildCls(typeArg.typeSignature)}]).encode(writer, $termName, encoderContext)
               }
             """
            } else if (cls.isPrimitive)
              q"registry.get(classOf[${toJavaPrimitive(sym.typeSignature.typeSymbol)}]).encode(writer, currentValue, encoderContext)"
            else if (cls.isSealed) {
              q"""currentValue match { case
                      ..${
                cls.knownDirectSubclasses.map(s => {
                  val name = TermName(c.freshName())
                  cq"$name: $s => registry.get(classOf[${buildCls(s.typeSignature)}]).encode(writer, $name, encoderContext)"
                }
                )
              }}
                   """
            } else q"registry.get(classOf[${buildCls(sym.typeSignature)}]).encode(writer, currentValue, encoderContext)"
          }
            """
        }
      }
            writer.writeEndDocument
           }

          override def decode(reader: BsonReader, decoderContext: DecoderContext): $ts = {
              val data = scala.collection.mutable.Map.empty[String, Any]
              reader.readStartDocument()
              while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
                reader.readName() match { case
                ..${
        getPrimaryCtorParams(ts).map(sym =>
          if (sym.typeSignature.typeSymbol.asClass.toType.<:<(typeOf[Option[Any]])) {
            val typeArg = sym.typeSignature.typeArgs.head.typeSymbol
            cq"""${sym.name.encodedName.toString} =>
                  val bsonValue = if(reader.getCurrentBsonType() == org.bson.BsonType.NULL) {
                    reader.skipValue()
                    None
                  } else Some(registry.get(classOf[${if (typeArg.asClass.isPrimitive) tq"${toJavaPrimitive(typeArg)}" else buildCls(typeArg.typeSignature)}]).decode(reader, decoderContext))
                  data += (${sym.name.encodedName.toString} -> bsonValue)
              """
          } else
            cq"""${sym.name.encodedName.toString} =>
                  data += (${sym.name.encodedName.toString} -> registry.get(classOf[${if (sym.typeSignature.typeSymbol.asClass.isPrimitive) tq"${toJavaPrimitive(sym.typeSignature.typeSymbol)}" else buildCls(sym.typeSignature)}]).decode(reader, decoderContext))""")
      }
                case _ => reader.skipValue()
                }
              }
              reader.readEndDocument()
              new $ts(..${getPrimaryCtorParams(ts).map(sym => q"data.get(${sym.name.encodedName.toString}).get.asInstanceOf[${buildCls(sym.typeSignature)}]")})
          }
         }
       """
    c.Expr[org.bson.codecs.Codec[A]](
      q"""
         $codec
         new $codecName($registry)
       """)
  }

  def createSealedCodecProviderImpl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[org.bson.codecs.configuration.CodecProvider] = {
    import c.universe._

    val ts = weakTypeOf[A].typeSymbol
    val providers = ts.asClass.knownDirectSubclasses.map { sym =>
      if (sym.typeSignature.typeSymbol.asClass.isModuleClass)
        q"""Macros.createConstCodecProvider[$sym](${sym.name.toTermName})"""
      else
        q"""Macros.createCodecProvider[$sym]"""
    }
    val code =
      q"""
         new SealedCodecProvider[$ts] {
            val codecProviders = Seq(..$providers)
            val classMapping = Map(..${ts.asClass.knownDirectSubclasses.map { sym => q"${sym.name.encodedName.toString} -> classOf[$sym]" }})

            override def get[T](clazz: Class[T], registry: CodecRegistry): Codec[T] = {
              if(classOf[$ts].isAssignableFrom(clazz)) new SealedCodec[T](clazz, registry).asInstanceOf[Codec[T]] else null
            }
         }
       """

    c.Expr[org.bson.codecs.configuration.CodecProvider](code)
  }

  def createConstCodecImpl[A: c.WeakTypeTag](c: blackbox.Context)(value: c.Expr[A]): c.Expr[org.bson.codecs.Codec[A]] = {
    import c.universe._

    val ts = weakTypeOf[A].typeSymbol
    val code =
      q"""
         new ConstCodec[$ts]($value) {
            override def getEncoderClass: Class[$ts] = classOf[$ts]
         }
       """
    c.Expr[org.bson.codecs.Codec[A]](code)
  }

  def createConstCodecProviderImpl[A: c.WeakTypeTag](c: blackbox.Context)(value: c.Expr[A]): c.Expr[org.bson.codecs.configuration.CodecProvider] = {
    import c.universe._

    val ts = weakTypeOf[A].typeSymbol.asClass
    val code =
      q"""
         new org.bson.codecs.configuration.CodecProvider {

            private lazy val codec = Macros.createConstCodec($value)

            override def get[T](clazz: Class[T], registry: CodecRegistry): Codec[T] =
              if (classOf[$ts].isAssignableFrom(clazz)) codec.asInstanceOf[Codec[T]] else null
         }
       """
    c.Expr[org.bson.codecs.configuration.CodecProvider](code)
  }


}