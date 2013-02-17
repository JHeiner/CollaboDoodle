
package code.comet

import scala.xml.{Elem,NodeSeq}
import net.liftweb.common.{Box,Full,Empty}
import net.liftweb.http.{SHtml,CometListener,ListenerManager,RenderOut}
import net.liftweb.http.{StatefulComet,CometState,DeltaTrait}
import net.liftweb.actor.LiftActor
import net.liftweb.http.js.{JsCmd,JsCmds}

case class Message( user:String, text:String, num:Int = -1 )
extends DeltaTrait
{
  def toJs:JsCmd = JsCmds.Run( "ChatAppend("+user+",'"+text+"');" )

  def trimThenIfNonEmpty( action:Message=>Unit ) {
	val trimmed = text.trim
	if ( trimmed.nonEmpty ) action(
	  if ( trimmed.length == text.length ) this
	  else Message( user, trimmed, num ) ) }
}

object History
{
  val empty = History( Nil )
  val reset:JsCmd = JsCmds.Run( "ChatReset();" )
}
case class History( messages:List[Message] )
extends CometState[Message,History]
with net.liftweb.common.Logger
{
  def size = if ( messages.isEmpty ) 0 else messages.head.num

  def +( m:Message ) = History( Message( m.user, m.text, size+1 ) :: messages )

  def -( previous:History ):Seq[Message] = {
	var (since,shared) = messages.splitAt( size - previous.size )
	if ( shared != previous.messages ) error( "!!! state difference wrong" )
	since.reverse }

  def render:NodeSeq = {
  	error( "!!! render should not be called, Chat renders the history" )
  	NodeSeq.Empty }

  def toJs:JsCmd =
	messages.foldRight( History.reset ) {
	  ( m, j ) => JsCmds.CmdPair( j, m.toJs ) }
}

class Chat
extends StatefulComet with CometListener
{
  type Delta = Message
  type State = History
  def emptyState = History.empty
  def testState( in:Any ):Box[State] = in match {
	case g:History => Full( g )
	case _ => Empty }

  def registerWith = ChatServer
  val user = ChatServer.userCount.incrementAndGet.toString
  def sendMessage( text:String ) =
	Message( user, text ).trimThenIfNonEmpty( ChatServer.! )

  override def render:RenderOut = Seq(
	SHtml.ajaxForm( SHtml.text( "", sendMessage, "id" -> "ChatInput" ),
					JsCmds.Noop, JsCmds.Run( "ChatInput.value = '';" ) ),
	JsCmds.Script( JsCmds.CmdPair(
	  JsCmds.Run( "ChatUser="+user+";" ), state.toJs )) )
}
object ChatServer
extends LiftActor with ListenerManager
{
  val userCount = new java.util.concurrent.atomic.AtomicInteger

  private var history = History.empty + Message( "0", "Welcome" )
  private def addMessage( m:Message ) { history += m ; updateListeners() }
  def createUpdate = history

  override def lowPriority = {
	case m:Message => m.trimThenIfNonEmpty( addMessage ) }
}
