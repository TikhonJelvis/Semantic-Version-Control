package code
package model

import net.liftweb._
import util._
import Helpers._
import common._
import json._

import scala.xml.Node

case class Commit(id: Int, parents: List[Int], tree: FileTree)
case class FileTree(id: Int, children: List[FileTree], file: Option[List[Expression]])
case class Expression(id: Int, `type`: List[String], value: String, body: Option[List[Expression]])
case class Diff(base: Commit, changeset: List[Change])
trait Change
case class Insertion(location: List[Int], body: Expression, operation: String = "insertion") extends Change
case class Deletion(location: List[Int], operation: String = "deletion") extends Change
case class Modification(location: List[Int], body: Expression, operation: String = "modification") extends Change

object Commit {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  private var commits: List[Commit] = parse(data).extract[List[Commit]]

  def apply(in: JValue): Box[Commit] = Helpers.tryo{in.extract[Commit]}

  def unapply(id: Int): Option[Commit] = Commit.find(id)

  def unapply(in: JValue): Option[Commit] = apply(in)

  def unapply(in: Any): Option[(Int, List[Int], FileTree)] = {
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
   { "id": 1, "children": [], "file":
     [{ "id": 1, "type": ["function"], "value": "(define foo)", "body":
       [{ "id": 1, "type": ["keyword"], "value": "define" },
        { "id": 2, "type": ["symbol"], "value": "foo" }]}]}},
 {"id": 2, "parents": [1], "tree":
    { "id": 1, "children": [], "file":
      [{ "id": 1, "type": ["function"], "value": "(define foo)", "body":
        [{ "id": 1, "type": ["keyword"], "value": "define" },
         { "id": 2, "type": ["symbol"], "value": "foo" },
         { "id": 3, "type": ["arguments"], "value": "()" }]}]}}]"""

  def find(id: Int): Box[Commit] = synchronized {
    commits.find(_.id == id)
  }

  def diff(id: Int): Box[Diff] = {
    Commit.find(id) match {
      case Full(commit) =>
        commit.parents match {
          case parentId :: _ =>
            Commit.find(parentId) match {
              case Full(parent) =>
                Full(Diff(parent, jsonDiff(FileTree.toJson(parent.tree), FileTree.toJson(commit.tree))))
              case _ =>
                Empty
            }
          case _ =>
            Empty
        }
      case _ =>
        Empty
    }
  }

  def jsonDiff(a: JValue, b: JValue): List[Change] = {
    // For now, just do a diff from the beginning
    val different = (a.children.zipWithIndex).zip(b.children.zipWithIndex).dropWhile {
      case ((x, i), (y, j)) => x == y
    }
    val deletions = different.map {
      case ((fromA, i), (fromB, j)) => Deletion(List(i))
    }.reverse
    val insertions = different.map {
      case ((fromA, i), (fromB, j)) => Insertion(List(j), Expression(fromB).open_!)
    }
    return deletions ++ insertions
  }

  def add(commit: Commit): Commit = {
    synchronized {
      commits = commit :: commits.filterNot(_.id == commit.id)
      commit
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

object Diff {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  def apply(in: JValue): Box[Diff] = Helpers.tryo{in.extract[Diff]}

  def unapply(in: JValue): Option[Diff] = apply(in)

  def unapply(in: Any): Option[(Commit, List[Change])] = {
    in match {
      case diff: Diff => Some((diff.base, diff.changeset))
      case _ => None
    }
  }

  implicit def toJson(diff: Diff): JValue =
    Extraction.decompose(diff)

  implicit def toJson(diffs: Seq[Diff]): JValue =
    Extraction.decompose(diffs)
}

object Change {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  def apply(in: JValue): Box[Change] = in \\ "operation" match {
    case JString("insertion") =>
      Helpers.tryo{in.extract[Insertion]}
    case JString("deletion") =>
      Helpers.tryo{in.extract[Deletion]}
    case JString("modification") =>
      Helpers.tryo(in.extract[Modification])
    case op =>
      throw new UnsupportedOperationException("Unknown operation %s".format(op))
  }

  implicit def toJson(change: Change): JValue =
    Extraction.decompose(change)

  implicit def toJson(changes: Seq[Change]): JValue =
    Extraction.decompose(changes)
}
