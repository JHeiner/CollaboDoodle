
package code.comet

import scala.xml.Elem
import net.liftweb.http.{CometActor,CometListener,ListenerManager,RenderOut}
import net.liftweb.actor.LiftActor

case class Gol(sgsm:List[String])
{
  def +(s:String) = Gol( s :: sgsm )

  def lis = sgsm.foldLeft(List.empty[Elem]) {
	(r,m) => <li>{m}</li> :: r }
}

class Chat extends CometActor with CometListener
{
  private var gol = Gol(Nil)

  def registerWith = ChatServer

  override def lowPriority = {
	case x:Gol => gol = x ; reRender() }

  def render = <ul>{ gol.lis }</ul>
}
object ChatServer extends LiftActor with ListenerManager
{
  private var gol = Gol(List("Welcome"))

  def createUpdate = gol

  override def lowPriority = {
	case s:String =>
	  val t = s.trim
	  if ( t.nonEmpty ) {
		gol += s ; updateListeners() } }
}
