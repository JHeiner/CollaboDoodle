
package code.comet

import scala.xml.{Elem,NodeSeq,Text}
import net.liftweb.common.{Box,Full,Empty}
import net.liftweb.http.{SHtml,CometListener,ListenerManager,RenderOut}
import net.liftweb.http.{StatefulComet,CometState,DeltaTrait}
import net.liftweb.actor.LiftActor
import net.liftweb.util.AltXML
import net.liftweb.http.js.{JsCmd,JsCmds,JE}
import JsCmds.jsExpToJsCmd

case class Message( user:String, nick:String, text:String, num:Int = -1 )
extends DeltaTrait
{
  def toJs:JsCmd = JE.Call( "ChatAppend", user, nick, text )

  def trimThenIfNonEmpty( action:Message=>Unit ) {
	val trimmed = text.trim
	if ( trimmed.nonEmpty ) action(
	  if ( trimmed.length == text.length ) this
	  else Message( user, nick, trimmed, num ) ) }
}

object History
{
  val empty = History( Nil )
  val reset:JsCmd = JE.Call( "ChatReset" )
}
case class History( messages:List[Message] )
extends CometState[Message,History]
with net.liftweb.common.Logger
{
  def size = if ( messages.isEmpty ) 0 else messages.head.num

  def +( m:Message ) = History(
	Message( m.user, m.nick, m.text, size+1 ) :: messages )

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
  var nick = ""
  def trimThenHandle( input:String ) = {
	val escaped = new StringBuilder( input.length )
	AltXML.toXML( Text(input), null, escaped, false, false )
	var i = escaped.length - 1
	while ( i >= 0 ) { escaped.charAt( i ) match {
	  // see owasp.org XSS prevention cheat sheet...
	  case '\'' => escaped.replace( i, i+1, "&#x27;" )
	  case '/' => escaped.replace( i, i+1, "&#x2F;" )
	  case _ => } ; i -= 1 }
	val text = escaped.result.trim
	i = text.indexOf(' ')
	if ( i < 0 ) i = text.length
	val before = text.substring( 0, i ).toLowerCase
	val after = text.substring( i ).trim
	if ( before match {
	  case "&#x2f;nick" =>
		val from = if ( nick.nonEmpty ) (" "+nick) else ""
		val into = if ( after == nick ) "" else (" \u27a1 "+after)
		ChatServer ! Message( "0", "/nick", user+from+into )
		nick = after
		false
	  case "&#x2f;time" =>
		if ( after.nonEmpty ) true else {
		  ChatServer ! ChatServer.timeStamp
		  false }
	  case _ => true }) ChatServer ! Message( user, nick, text ) }

  override def render:RenderOut = Seq(
	SHtml.ajaxForm( SHtml.text( "", trimThenHandle, "id" -> "ChatInput" ),
					JsCmds.Noop, JsCmds.SetValById( "ChatInput", "" ) ),
	JsCmds.Script( JsCmds.CmdPair(
	  JsCmds.SetExp( JE.JsRaw( "ChatUser" ), user ), state.toJs )) )
}
object ChatServer
extends LiftActor with ListenerManager
{
  val userCount = new java.util.concurrent.atomic.AtomicInteger
  val dateRFC822 = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  val dateSimple = new java.text.SimpleDateFormat( dateRFC822 )
  def dateCurrent = dateSimple.format( new java.util.Date )
  def timeStamp = Message( "0", "/time", dateCurrent )

  private var history = History.empty + timeStamp
  private def addMessage( m:Message ) { history += m ; updateListeners() }
  def createUpdate = history

  override def lowPriority = {
	case m:Message => m.trimThenIfNonEmpty( addMessage ) }
}
