package code
package lib

import model._

import net.liftweb._
import common._
import http._

/**
 * A simple example of a REST style interface
 * using the basic Lift tools
 */
object BasicExample {
  lazy val findItem: LiftRules.DispatchPF = {
    case Req(commitId :: "show" :: Nil, suffix, GetRequest) => 
      () => Box(JsonResponse("hello"))
  }
}
