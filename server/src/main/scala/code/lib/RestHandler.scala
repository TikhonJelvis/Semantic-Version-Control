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

  serve( List("commit") prefix {
/*
    // /api/item returns all the items
    case Nil JsonGet _ => Item.inventoryItems: JValue

    // /api/item/count gets the item count
    case "count" :: Nil JsonGet _ => JInt(Item.inventoryItems.length)
*/

    case "id" :: id :: Nil Get _ =>
      for {
        commit <- Commit.find(id.toInt)
      } yield commit: JValue

    case Nil JsonPut Commit(c) -> _ => Commit.add(c): JValue

/*
    // /api/item/search/foo or /api/item/search?q=foo
    case "search" :: q JsonGet _ =>
      (for {
        searchString <- q ::: S.params("q")
        item <- Item.search(searchString)
      } yield item).distinct: JValue

    // DELETE the item in question
    case Item(item) :: Nil JsonDelete _ =>
      Item.delete(item.id).map(a => a: JValue)

    // PUT adds the item if the JSON is parsable
    case Nil JsonPut Item(item) -> _ => Item.add(item): JValue

    // POST if we find the item, merge the fields from the
    // the POST body and update the item
    case Item(item) :: Nil JsonPost json -> _ =>
      Item(mergeJson(item, json)).map(Item.add(_): JValue)

    // Wait for a change to the Items
    // But do it asynchronously
    case "change" :: Nil JsonGet _ =>
      RestContinuation.async {
        satisfyRequest => {
          // schedule a "Null" return if there's no other answer
          // after 110 seconds
          Schedule.schedule(() => satisfyRequest(JNull), 110 seconds)

          // register for an "onChange" event.  When it
          // fires, return the changed item as a response
          Item.onChange(item => satisfyRequest(item: JValue))
        }
      }
*/
  })
}
