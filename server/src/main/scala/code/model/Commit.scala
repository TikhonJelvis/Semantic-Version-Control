package code
package model

import net.liftweb._
import util._
import Helpers._
import common._
import json._

import scala.xml.Node

case class Commit(id: Int, parents: List[Int], body: List[Expression])
case class Expression(id: Int, `type`: String, value: String, body: List[Expression])
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

  def unapply(in: Any): Option[(Int, List[Int], List[Expression])] = {
    in match {
      case commit: Commit => Some((commit.id, commit.parents, commit.body))
      case _ => None
    }
  }

  implicit def toJson(commit: Commit): JValue =
    Extraction.decompose(commit)

  implicit def toJson(commits: Seq[Commit]): JValue =
    Extraction.decompose(commits)

  def allCommits: Seq[Commit] = commits

  private def data = "[]"
// """
// [{"id": 1, "parents": [], "tree":
//    { "id": 1, "children": [], "file":
//      [{ "id": 1, "type": ["function"], "value": "(define foo)", "body":
//        [{ "id": 1, "type": ["keyword"], "value": "define" },
//         { "id": 2, "type": ["symbol"], "value": "foo" }]}]}},
//  {"id": 2, "parents": [1], "tree":
//     { "id": 1, "children": [], "file":
//       [{ "id": 1, "type": ["function"], "value": "(define foo (x) (+ x 1))", "body":
//         [{ "id": 1, "type": ["keyword"], "value": "define" },
//          { "id": 2, "type": ["symbol"], "value": "foo" },
//          { "id": 3, "type": ["arguments"], "value": "()", "body": [] }]},
//        { "id": 2, "type": ["function"], "value": "(define bar)", "body":
//         [{ "id": 1, "type": ["keyword"], "value": "define" },
//          { "id": 2, "type": ["symbol"], "value": "bar" }]}]}},
//  {"id": 3, "parents": [1], "tree":
//     { "id": 1, "children": [], "file":
//       [{ "id": 1, "type": ["function"], "value": "(define foo)", "body":
//         [{ "id": 1, "type": ["keyword"], "value": "define" },
//          { "id": 2, "type": ["symbol"], "value": "foo" }]},
//        { "id": 2, "type": ["comment"], "value": "; hello world!" }]}}]"""

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
                Full(Diff(parent, exprListDiff(parent.body, commit.body, List(0))))
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

  def exprListDiff(a: List[Expression], b: List[Expression], pos: List[Int]): List[Change] = (a, b) match {
    case (Nil, Nil) =>
      List()
    case (a :: Nil, Nil) =>
      List(Deletion(pos))
    case (Nil, a :: Nil) =>
      List(Insertion(pos, a))
    case (a :: aTail, b :: bTail) =>
      val newPos = pos.dropRight(1) ++ (pos.takeRight(1).map(x => x + 1))
      exprDiff(a, b, pos) ++ exprListDiff(aTail, bTail, newPos)
  }

  def exprDiff(a: Expression, b: Expression, pos: List[Int]) : List[Change] = (a, b) match {
    case (Expression(idA, typeA, _, bodyA), Expression(idB, typeB, _, bodyB)) if idA == idB && typeA == typeB =>
      exprListDiff(bodyA, bodyB, pos :+ 0)
    case _ =>
      List(Modification(pos, b))
  }

  def add(commit: Commit): Commit = {
    synchronized {
      commits = commit :: commits.filterNot(_.id == commit.id)
      commit
    }
  }

  def add(exprs: List[Expression]): Commit = {
    synchronized {
      val (commitId, parents) = commits match {
        case Nil => (1, List())
        case _ => (commits.last.id + 1, List(commits.last.id))
      }
      val commit = Commit(commitId, parents, exprs)
      Commit.add(commit)
    }
  }

  def search(str: String): List[Commit] = {
    throw new UnsupportedOperationException
  }
}

object Expression {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  def apply(in: JValue): Box[Expression] = Helpers.tryo{in.extract[Expression]}

  def unapply(in: JValue): Option[Expression] = apply(in)

  def unapply(in: Any): Option[(Int, String, String, List[Expression])] = {
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
