package code
package model

import net.liftweb._
import util._
import Helpers._
import common._
import json._

import scala.xml.Node

case class Commit(id: Int, parents: List[Int], tree: Option[FileTree])
case class FileTree(id: Int, children: List[FileTree], file: Option[List[Expression]])
case class Expression(id: Int, `type`: List[String], value: String, body: Option[List[Expression]])

object Commit {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  private var commits: List[Commit] = parse(data).extract[List[Commit]]

  def apply(in: JValue): Box[Commit] = Helpers.tryo{in.extract[Commit]}

  def unapply(id: Int): Option[Commit] = Commit.find(id)

  def unapply(in: JValue): Option[Commit] = apply(in)

  def unapply(in: Any): Option[(Int, List[Int], Option[FileTree])] = {
    in match {
      case commit: Commit => Some((commit.id, commit.parents, commit.tree))
      case _ => None
    }
  }

  implicit def toJson(commit: Commit): JValue =
    Extraction.decompose(commit)

  implicit def toJson(commits: Seq[Commit]): JValue =
    Extraction.decompose(commits)

  def allCommits: Seq[Commit] = commits

  private def data =
"""
[{"id": 1, "parents": [], "tree":
   { "id": 2, "children": [], "file":
     [{ "id": 3, "type": ["function"], "value": "(define foo)", "body":
       [{ "id": 4, "type": ["keyword"], "value": "define" },
        { "id": 5, "type": ["symbol"], "value": "foo" }]}]}}]"""

  def find(id: Int): Box[Commit] = synchronized {
    commits.find(_.id == id)
  }

  def add(commit: Commit) {
    synchronized {
      commits = commit :: commits.filterNot(_.id == commit.id)
    }
  }

  def search(str: String): List[Commit] = {
    throw new UnsupportedOperationException
  }
}

object FileTree {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  def apply(in: JValue): Box[FileTree] = Helpers.tryo{in.extract[FileTree]}

  def unapply(in: JValue): Option[FileTree] = apply(in)

  def unapply(in: Any): Option[(Int, List[FileTree], Option[List[Expression]])] = {
    in match {
      case fileTree: FileTree => Some((fileTree.id, fileTree.children, fileTree.file))
      case _ => None
    }
  }

  implicit def toJson(fileTree: FileTree): JValue =
    Extraction.decompose(fileTree)

  implicit def toJson(fileTrees: Seq[FileTree]): JValue =
    Extraction.decompose(fileTrees)
}

object Expression {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  def apply(in: JValue): Box[Expression] = Helpers.tryo{in.extract[Expression]}

  def unapply(in: JValue): Option[Expression] = apply(in)

  def unapply(in: Any): Option[(Int, List[String], String, Option[List[Expression]])] = {
    in match {
      case expr: Expression => Some((expr.id, expr.`type`, expr.value, expr.body))
      case _ => None
    }
  }

  implicit def toJson(expr: Expression): JValue =
    Extraction.decompose(expr)

  implicit def toJson(exprs: Seq[Expression]): JValue =
    Extraction.decompose(exprs)
}
