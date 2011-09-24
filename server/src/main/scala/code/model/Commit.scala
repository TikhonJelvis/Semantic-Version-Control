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
case class Diff(base: Commit, changeset: List[Change], annotations: List[Annotation])
trait Change {
  def location: List[Int]
  def body: Expression
  def operation: String
}
case class Insertion(location: List[Int], body: Expression, operation: String = "insertion") extends Change
case class Deletion(location: List[Int], body: Expression, operation: String = "deletion") extends Change
case class Modification(location: List[Int], oldBody: Expression, body: Expression, operation: String = "modification") extends Change
trait Annotation
case class OnlyComments(annotation: String = "onlyComments") extends Annotation

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

  private def data = """
[{
  "id":2,
  "parents":[1],
  "body":[{
    "id":5,
    "type":"list",
    "value":"(define (id n) n)",
    "body":[{
      "id":3,
      "type":"keyword",
      "value":"lambda",
      "body":[]
    },{
      "id":5,
      "type":"list",
      "value":"",
      "body":[{
        "id":3,
        "type":"variable",
        "value":"id",
        "body":[]
      },{
        "id":4,
        "type":"variable",
        "value":"n",
        "body":[]
      }]
    },{
      "id":4,
      "type":"variable",
      "value":"n",
      "body":[]
    }]
  },{
    "id":6,
    "type":"comment",
    "value":"hello",
    "body":[]
  }]
},{
  "id":1,
  "parents":[],
  "body":[{
    "id":5,
    "type":"list",
    "value":"(define (id n) n)",
    "body":[{
      "id":3,
      "type":"keyword",
      "value":"lambda",
      "body":[]
    },{
      "id":5,
      "type":"list",
      "value":"",
      "body":[{
        "id":3,
        "type":"variable",
        "value":"id",
        "body":[]
      },{
        "id":4,
        "type":"variable",
        "value":"n",
        "body":[]
      }]
    },{
      "id":4,
      "type":"variable",
      "value":"n",
      "body":[]
    }]
  }]
}]
"""

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
                val changes = exprListDiff(parent.body, commit.body, List(0))
                Full(Diff(parent, changes, annotate(changes, parent)))
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

  def annotate(changes: List[Change], parent: Commit): List[Annotation] = {
    if (changes.forall {
      case Modification(_, oldBody, body, _) =>
        body.`type` == "comment" && oldBody.`type` == "comment"
      case c: Change =>
        c.body.`type` == "comment"
    }) {
      List(OnlyComments())
    } else {
      List()
    }
  }

  def exprListDiff(a: List[Expression], b: List[Expression], pos: List[Int]): List[Change] = (a, b) match {
    case (Nil, Nil) =>
      List()
    case (a :: Nil, Nil) =>
      List(Deletion(pos, a))
    case (Nil, a :: Nil) =>
      List(Insertion(pos, a))
    case (a :: aTail, Nil) =>
      val newPos = pos.dropRight(1) ++ (pos.takeRight(1).map(x => x + 1))
      List(Deletion(pos, a)) ++ exprListDiff(aTail, Nil, newPos)
    case (Nil, a :: aTail) =>
      val newPos = pos.dropRight(1) ++ (pos.takeRight(1).map(x => x + 1))
      List(Insertion(pos, a)) ++ exprListDiff(Nil, aTail, newPos)
    case (a :: aTail, b :: bTail) =>
      val newPos = pos.dropRight(1) ++ (pos.takeRight(1).map(x => x + 1))
      exprDiff(a, b, pos) ++ exprListDiff(aTail, bTail, newPos)
  }

  def exprDiff(a: Expression, b: Expression, pos: List[Int]) : List[Change] = (a, b) match {
    case (Expression(idA, typeA, valueA, bodyA), Expression(idB, typeB, valueB, bodyB)) if idA == idB && typeA == typeB && valueA == valueB =>
      exprListDiff(bodyA, bodyB, pos :+ 0)
    case _ =>
      List(Modification(pos, a, b))
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
        case _ => (commits.head.id + 1, List(commits.head.id))
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

  def unapply(in: Any): Option[(Commit, List[Change], List[Annotation])] = {
    in match {
      case diff: Diff => Some((diff.base, diff.changeset, diff.annotations))
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

object Annotation {
  private implicit val formats =
    net.liftweb.json.DefaultFormats

  def apply(in: JValue): Box[Annotation] = in \\ "annotation" match {
    case JString("onlyComments") =>
      Helpers.tryo{in.extract[OnlyComments]}
    case op =>
      throw new UnsupportedOperationException("Unknown operation %s".format(op))
  }

  implicit def toJson(change: Annotation): JValue =
    Extraction.decompose(change)

  implicit def toJson(changes: Seq[Annotation]): JValue =
    Extraction.decompose(changes)
}
