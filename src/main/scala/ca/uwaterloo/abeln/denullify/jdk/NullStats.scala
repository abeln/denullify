package ca.uwaterloo.abeln.denullify.jdk

import java.util.jar.{JarEntry, JarFile}
import java.io.PrintWriter

import org.objectweb.asm._
import org.objectweb.asm.commons.Method
import org.objectweb.asm.tree.{ClassNode, FieldNode, MethodNode, TypeAnnotationNode}

import scala.collection.JavaConverters._
import play.api.libs.json._

object NullStats {

  case class ClassStats(name: String, fields: Seq[FieldStats], methods: Seq[MethodStats])
  case class FieldStats(name: String, desc: String, nnTpe: Boolean)
  case class MethodStats(name: String, desc: String, numParams: Int, nnParams: Seq[Int], nnRet: Boolean)

  implicit val fieldWrites: Writes[FieldStats] = Json.writes[FieldStats]
  implicit val methodWrites: Writes[MethodStats] = Json.writes[MethodStats]
  implicit val classWrites: Writes[ClassStats] = Json.writes[ClassStats]

  def entry(): Unit = {
    val jarFile = new JarFile("lib/jdk8-2.6.0.jar")
    val entries = jarFile.stream()
    val classStats: Seq[ClassStats] = entries.filter(_.getName.endsWith(".class")).map[ClassStats] {
      entry => stats(jarFile, entry)
    }.iterator().asScala.toSeq
    new PrintWriter("explicit-nulls-stdlib.json") { write(Json.prettyPrint(Json.toJson(classStats))); close }
  }

  def stats(jar: JarFile, entry: JarEntry): ClassStats = {
    val reader = new ClassReader(jar.getInputStream(entry))
    val classNode = new ClassNode()
    reader.accept(classNode, 0)
    val fStats = classNode.fields.asScala.filterNot(privateField).map(fieldStats).filter(_.nnTpe)
    val mStats = classNode.methods.asScala.filterNot(privateMethod).map(methodStats).filter(isNonNullMethod)
    ClassStats(classNode.name, fStats, mStats)
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
    method.nnParams.nonEmpty || method.nnParams.nonEmpty
  }

  def fromJava[T](lst: java.util.List[T]): Seq[T] = {
    if (lst == null) Seq.empty[T] else lst.asScala
  }
}