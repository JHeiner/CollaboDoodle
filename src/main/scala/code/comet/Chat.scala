
package code.comet

import net.liftweb.http.{CometActor,CometListener,ListenerManager,RenderOut}
import net.liftweb.actor.LiftActor
import net.liftweb.util.ClearClearable

case class Gol(sgsm:List[String])
{
  def +(s:String) = Gol( s :: sgsm )
}

class Chat extends CometActor with CometListener
{
  private var gol = Gol(Nil)

  def registerWith = ChatServer

  override def lowPriority = {
	case x:Gol => gol = x ; reRender() }

  def render = "li *" #> gol.sgsm & ClearClearable
}
object ChatServer extends LiftActor with ListenerManager
{
  private var gol = Gol(List("Welcome"))

  def createUpdate = gol

  override def lowPriority = {
	case s:String => gol += s ; updateListeners() }
}
