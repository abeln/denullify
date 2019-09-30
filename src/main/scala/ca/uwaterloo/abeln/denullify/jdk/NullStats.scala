package ca.uwaterloo.abeln.denullify.jdk

import java.util.jar.{JarEntry, JarFile}
import java.io.{File, FileOutputStream, PrintWriter}

import org.objectweb.asm._
import org.objectweb.asm.commons.Method
import org.objectweb.asm.tree.{ClassNode, FieldNode, MethodNode, TypeAnnotationNode}

import scala.collection.JavaConverters._
import play.api.libs.json._

object NullStats {

  var fieldCount: Int = 0
  var methodCount: Int = 0
  var classCount: Int = 0

  case class ClassStats(name: String, fields: Seq[FieldStats], methods: Seq[MethodStats])
  case class FieldStats(name: String, desc: String, nnTpe: Boolean)
  case class MethodStats(name: String, desc: String, numParams: Int, nnParams: Seq[Int], nnRet: Boolean)

  def entry(): Unit = {
    val jarFile = new JarFile("lib/jdk8-2.6.0.jar")
    val entries = jarFile.stream()
    val classStats: Seq[ClassStats] = entries.filter(_.getName.endsWith(".class")).map[ClassStats] {
      entry => stats(jarFile, entry)
    }.iterator().asScala.toSeq
    // printJson(classStats)
    // printXml(classStats)
    printText(classStats)
    println(s"classes: $classCount")
    println(s"fields: $fieldCount")
    println(s"methods: $methodCount")
  }

  def printText(classStats: Seq[ClassStats]): Unit = {
    val writer = new PrintWriter(new File("explicit-nulls-meta.txt"))

    val SPACE = 2
    def tab(): Unit = writer.write(" " * SPACE)

    def printFields(fields: Seq[FieldStats]): Unit = {
      writer.println(fields.size)
      fields foreach { field =>
        tab(); writer.println(field.name)
        tab(); tab(); writer.println(field.desc)
        tab(); tab(); writer.println(field.nnTpe)
      }
    }

    def printMethods(methods: Seq[MethodStats]): Unit = {
      writer.println(methods.size)
      methods foreach { method =>
        tab(); writer.println(method.name)
        tab(); tab(); writer.println(method.desc)
        tab(); tab(); writer.println(method.nnRet)
        //tab(); tab(); writer.println(method.nnParams.size)
        //method.nnParams foreach { p =>
        //  tab(); tab(); tab(); writer.println(p)
        //}
      }
    }

    try {
      writer.println(classStats.size)
      classStats foreach { stats =>
        writer.println(stats.name)
        printFields(stats.fields)
        printMethods(stats.methods)
      }
    } finally {
      writer.close()
    }
  }

  def printXml(classStats: Seq[ClassStats]): Unit = {
    def classToXml(clazz: ClassStats): scala.xml.Elem = {
      <class>
        <name>{clazz.name}</name>
        <fields>{clazz.fields.map(fieldToXml)}</fields>
        <methods>{clazz.methods.map(methodToXml)}</methods>
      </class>
    }

    def fieldToXml(field: FieldStats): scala.xml.Elem = {
      <field>
        <name>{field.name}</name>
        <desc>{field.desc}</desc>
        <ret>{field.nnTpe}</ret>
      </field>
    }
    def methodToXml(method: MethodStats): scala.xml.Elem = {
      <method>
        <name>{method.name}</name>
        <desc>{method.desc}</desc>
        <params>{method.nnParams.map(p => <param>{p}</param>)}</params>
        <ret>{method.nnRet}</ret>
      </method>
    }

    val xml = <null-stats>{classStats.map(classToXml)}</null-stats>
//    scala.xml.XML.save("explicit-nulls-stdlib.xml", xml)
    val pp = new scala.xml.PrettyPrinter(100, 4)
    // TODO(abeln): print out encoding and other xml metadata?
    new PrintWriter("explicit-nulls-stdlib.xml") { write(pp.format(xml)); close }
  }

  def printJson(classStats: Seq[ClassStats]): Unit = {
    implicit val fieldWrites: Writes[FieldStats] = Json.writes[FieldStats]
    implicit val methodWrites: Writes[MethodStats] = Json.writes[MethodStats]
    implicit val classWrites: Writes[ClassStats] = Json.writes[ClassStats]
    new PrintWriter("explicit-nulls-stdlib.json") { write(Json.prettyPrint(Json.toJson(classStats))); close }
  }

  def stats(jar: JarFile, entry: JarEntry): ClassStats = {
    val reader = new ClassReader(jar.getInputStream(entry))
    val classNode = new ClassNode()
    reader.accept(classNode, 0)
    val fStats = classNode.fields.asScala.filterNot(privateField).map(fieldStats).filter(_.nnTpe)
    val mStats = classNode.methods.asScala.filterNot(privateMethod).map(methodStats).filter(isNonNullMethod)
    fieldCount += fStats.length
    methodCount += mStats.length
    if (fieldCount + methodCount > 0) classCount += 1
    // TODO(abeln): return an option, and only return stats if there's at least one field or method with useful
    // info.
    ClassStats(fqName(classNode.name), fStats, mStats)
  }

  def fqName(name: String): String = {
    name.replace("/", ".")
  }

  def privateField(field: FieldNode): Boolean = {
    field.access == Opcodes.ACC_PRIVATE
  }

  def privateMethod(method: MethodNode): Boolean = {
    method.access == Opcodes.ACC_PRIVATE
  }

  def fieldStats(field: FieldNode): FieldStats = {
    val tpeAnns = typeAnnots(field)
    val nn = tpeAnns.exists(isNonNull)
    if (nn) {
      assert(!tpeAnns.exists(isNullable), s"field ${field.name} is marked as both nullable and non-nullable")
    }
    FieldStats(field.name, field.desc, nn)
  }

  def methodStats(method: MethodNode): MethodStats = {
    val tpeAnns = typeAnnots(method)
    val (nnParams, nnRet) = tpeAnns.foldLeft(Seq.empty[Int], false) {
      case (acc@(accParams, accRet), ann) =>
        if (isNonNull(ann) && ann.typePath == null) {
          val tref = new TypeReference(ann.typeRef)
          tref.getSort match {
            case TypeReference.METHOD_RETURN =>
              (accParams, true)
            case TypeReference.METHOD_FORMAL_PARAMETER =>
              (accParams :+ tref.getFormalParameterIndex, accRet)
            case _ => acc
          }
        } else {
          acc
        }
    }
    val meth = new Method(method.name, method.desc)
    MethodStats(method.name, method.desc, meth.getArgumentTypes.length, nnParams, nnRet)
  }

  def typeAnnots(field: FieldNode): Seq[TypeAnnotationNode] = {
    fromJava(field.visibleTypeAnnotations) ++ fromJava(field.invisibleTypeAnnotations)
  }

  def typeAnnots(method: MethodNode): Seq[TypeAnnotationNode] = {
    fromJava(method.visibleTypeAnnotations) ++ fromJava(method.invisibleTypeAnnotations)
  }

  def isNonNull(annot: TypeAnnotationNode): Boolean = {
    annot.desc.contains("NonNull") && annot.typePath == null
  }

  def isNullable(annot: TypeAnnotationNode): Boolean = {
    annot.desc.contains("Nullable") && annot.typePath == null
  }

  def isNonNullMethod(method: MethodStats): Boolean = {
    method.nnParams.nonEmpty || method.nnRet
  }

  def fromJava[T](lst: java.util.List[T]): Seq[T] = {
    if (lst == null) Seq.empty[T] else lst.asScala
  }
}