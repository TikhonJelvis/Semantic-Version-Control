package code
package lib

import model._

import net.liftweb._
import common._
import http._
import rest._
import util._
import Helpers._
import json._
import scala.xml._

object RestHandler extends RestHelper {

  serve {
    case "commit" :: id :: Nil Get _ =>
      if (id.isEmpty)
        Commit.allCommits: JValue
      else
        for {
          commit <- Commit.find(id.toInt)
        } yield commit: JValue

    case "commit" :: Nil JsonPut Commit(c) -> _ => Commit.add(c): JValue

    case "commit" :: Nil JsonPost exprs -> _ => {
      println(exprs)
      Commit.add(exprs.extract[List[Expression]]): JValue
    }

    case "diff" :: id :: Nil Get _ =>
      for {
        diff <- Commit.diff(id.toInt)
      } yield diff: JValue

    case "commit" :: Nil Get _ => {
      Commit.allCommits: JValue
    }

    // Commit list
    // Diff 
  }
}
