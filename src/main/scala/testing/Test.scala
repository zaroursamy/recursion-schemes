package testing

import io.circe._
import io.circe.parser._
import matryoshka.Algebra
import matryoshka.data.Fix
import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.apache.avro.generic.GenericData
import scalaz.Functor
import matryoshka.implicits._

import scala.collection.JavaConverters._
import scala.util.Try

object Test extends App {

  val strRawData = s"""{"nom":"Toto", "age":null, "ts": 1234.1234, "adresse": {"num":1, "rue": "rue de la paix"}}"""
  val jsonRawData = parse(strRawData).getOrElse(Json.Null)
  val ruleJsonSchemaR: Fix[SchemaR] = Fix(StructR(Map("nom" -> Fix(StringR()), "age" -> Fix(IntR(true)), "ts" -> Fix(DoubleR()), "rue" -> Fix(StructR(Map("num"->Fix(IntR()), "rue" -> Fix(StringR())))))))

  /* FUNCTOR */
  implicit val schemaRFunctor: Functor[SchemaR] = new Functor[SchemaR] {
    override def map[A, B](fa: SchemaR[A])(f: A ⇒ B): SchemaR[B] = fa match {
      case StructR(fields) ⇒ StructR(fields.map { case (k, v) ⇒ k -> f(v) })
      case ArrayR(e)       ⇒ ArrayR(f(e))
      case IntR(b)         ⇒ IntR[B](b)
      case StringR(b)      ⇒ StringR[B](b)
      case LongR(b)        ⇒ LongR[B](b)
      case DoubleR(b)      ⇒ DoubleR[B](b)
    }
  }

  val algebra: Algebra[SchemaR, Schema] = {
    case IntR(b)         ⇒ if (b) Schema.createUnion(Schema.create(Schema.Type.INT), Schema.create(Schema.Type.NULL)) else Schema.create(Schema.Type.INT)
    case StringR(b)      ⇒ if (b) Schema.createUnion(Schema.create(Schema.Type.STRING), Schema.create(Schema.Type.NULL)) else Schema.create(Schema.Type.STRING)
    case LongR(b)        ⇒ if (b) Schema.createUnion(Schema.create(Schema.Type.LONG), Schema.create(Schema.Type.NULL)) else Schema.create(Schema.Type.LONG)
    case DoubleR(b)      ⇒ if (b) Schema.createUnion(Schema.create(Schema.Type.DOUBLE), Schema.create(Schema.Type.NULL)) else Schema.create(Schema.Type.DOUBLE)
    case BooleanR(b)     ⇒ if (b) Schema.createUnion(Schema.create(Schema.Type.BOOLEAN), Schema.create(Schema.Type.NULL)) else Schema.create(Schema.Type.BOOLEAN)
    case ArrayR(b)       ⇒ b
    case StructR(fields) ⇒ Schema.createRecord(fields.map { case (name, schema) ⇒ new Field(name, schema) }.toList.asJava)
  }

  val schema: Schema = ruleJsonSchemaR.cata[Schema](algebra)

  def getFieldJson(f: Field, json: Json) = {
    val fieldType: Schema.Type = f.schema.getType

    def mapFT(ft: Schema.Type): Any = ft match {
      case Schema.Type.STRING  ⇒ json.hcursor.get[String](f.name).toOption.getOrElse(null)
      case Schema.Type.INT     ⇒ json.hcursor.get[Int](f.name).toOption.getOrElse(null)
      case Schema.Type.LONG    ⇒ json.hcursor.get[Long](f.name).toOption.getOrElse(null)
      case Schema.Type.DOUBLE  ⇒ json.hcursor.get[Double](f.name).toOption.getOrElse(null)
      case Schema.Type.BOOLEAN ⇒ json.hcursor.get[Boolean](f.name).toOption.getOrElse(null)
      case Schema.Type.NULL    ⇒ null
      case Schema.Type.UNION   ⇒ f.schema.getTypes.asScala.flatMap(sc ⇒ Try(mapFT(sc.getType)).toOption).find(x ⇒ x != null).getOrElse(null)
      case Schema.Type.ARRAY   ⇒ json.hcursor.get[Array[Int]](f.name).toOption.getOrElse(Nil)
    }
    mapFT(fieldType)

  }

  def createRecord(schema: Schema, json: Json): GenericData.Record = {
    val avroRecord = new GenericData.Record(schema)
    schema.getFields.asScala.foreach { f: Field ⇒
      avroRecord.put(f.name, getFieldJson(f, json))
    }

    avroRecord
  }

  val avroRecord = createRecord(schema, jsonRawData)
  println(s"SCHEMA AVRO: \n$schema")
  println(avroRecord)

}


object TestT extends App{

 def intToBase(num: Double, base: Double): Array[Double] = {
   if(base<1) null
   else{
     var result = Array[Double]()
     if(num<1) result
     else{
       val quotient: Double = Math.floor(num/base)
       val remainder = num%base
       intToBase(quotient, base)++Array(remainder)
     }
   }
 }

  intToBase(6,2).foreach(println)


}
