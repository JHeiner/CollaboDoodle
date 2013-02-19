
package code.comet

import scala.xml.{Elem,NodeSeq,Text}
import net.liftweb.common.{Box,Full,Empty}
import net.liftweb.http.{SHtml,CometListener,ListenerManager,RenderOut}
import net.liftweb.http.{StatefulComet,CometState,DeltaTrait}
import net.liftweb.actor.LiftActor
import net.liftweb.util.AltXML
import net.liftweb.http.js.{JsCmd,JsCmds,JE}
import JsCmds.jsExpToJsCmd

case class Message( user:String, command:String, nick:String, text:String,
                    num:Int = -1 )
extends DeltaTrait
{
  def toJs:JsCmd = JE.Call( "ChatAppend", user, command, nick, text )
}

object History
{
  val empty = History( Nil )
}
case class History( messages:List[Message] )
extends CometState[Message,History]
with net.liftweb.common.Logger
{
  def size = if ( messages.isEmpty ) 0 else messages.head.num

  def +( m:Message ) = History( m.copy( num = size+1 ) :: messages )

  def -( previous:History ):Seq[Message] = {
    var (since,shared) = messages.splitAt( size - previous.size )
    if ( shared != previous.messages ) error( "!!! state difference wrong" )
    since.reverse }

  def render:NodeSeq = {
    error( "!!! render should not be called, Chat renders the history" )
    NodeSeq.Empty }

  def toJs( user:String ):JsCmd = {
	val setUser:JsCmd = JsCmds.JsCrVar( "ChatUser", user )
    messages.foldRight( setUser ) {
      ( m, j ) => JsCmds.CmdPair( j, m.toJs ) } }
}

case class Input( chat:Chat, dangerous:String )

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
  def sendInput( s:String ) { ChatServer ! Input( this, s ) }

  override def render:RenderOut = Seq(
    SHtml.ajaxForm( SHtml.text( "", sendInput, "id"->"ChatInput" ),
	                JsCmds.Noop, JE.JsRaw( "ChatInput.value=''" ) ),
    SHtml.ajaxForm( SHtml.hidden( sendInput _, "doodle", "id"->"ChatHidden" ),
	                JsCmds.Noop, JE.JsRaw( "ChatCheck()" ) ),
    JsCmds.Script( state.toJs( user ) ) )
}
object ChatServer
extends LiftActor with ListenerManager
{
  val userCount = new java.util.concurrent.atomic.AtomicInteger

  def userMessage( chat:Chat, text:String ) =
    Message( chat.user, "", chat.nick, text )
  def adminMessage( command:String, user:String, info:String ) =
    Message( "0", command, user, info )

  val dateRFC822 = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  val dateSimple = new java.text.SimpleDateFormat( dateRFC822 )
  def dateCurrent = dateSimple.format( new java.util.Date )
  def timeMessage( user:String ) = adminMessage( "/time", user, dateCurrent )

  def traceMessage( user:String ) = {
    import java.net.NetworkInterface._
    import scala.collection.JavaConversions._
    val (n4,n6) = ( for {
      i <- getNetworkInterfaces
      if ! i.isLoopback
      a <- i.getInetAddresses }
      yield a ).toList.partition{ _.isSiteLocalAddress }
    val ns = ( if ( n4.nonEmpty ) n4 else n6 ).map{ _.getHostAddress }
    adminMessage( "/trace", user, ns.mkString( ", " ) ) }

  private var history = History.empty + timeMessage("0") + traceMessage("0")
  private def addMessage( m:Message ) { history += m ; updateListeners() }
  def createUpdate = history

  override def lowPriority = {
    case Input( chat, dangerous ) =>
      val trimmed = escape( dangerous ).trim
      var space = trimmed.indexOf(' ')
      if ( space < 0 ) space = trimmed.length
      val before = trimmed.substring( 0, space ).toLowerCase
      val after = trimmed.substring( space ).trim
      val admin = before match {
        case "&#x2f;nick" =>
          val nick = chat.nick
          val into = if ( after == nick ) "" else (" \u27a1 "+after)
          if ( into.nonEmpty ) chat.nick = after
          Some( adminMessage( "/nick", chat.user, nick+into ) )
        case "&#x2f;time" =>
          if ( after.nonEmpty ) None else
            Some( timeMessage( chat.user ) )
        case "&#x2f;trace" =>
          if ( after.nonEmpty ) None else
            Some( traceMessage( chat.user ) )
        case _ => None }
      if ( admin.nonEmpty )
        addMessage( admin.get ) // admin might have empty text
      else if ( trimmed.nonEmpty ) // but empty user messages are dropped
        addMessage( userMessage( chat, trimmed ) ) }

  def escape( dangerous:String ) = {
    val builder = new StringBuilder( dangerous.length )
    AltXML.toXML( Text(dangerous), null, builder, false, false )
    var i = builder.length - 1
    while ( i >= 0 ) { builder.charAt( i ) match {
      // see owasp.org XSS prevention cheat sheet...
      case '\'' => builder.replace( i, i+1, "&#x27;" )
      case '/' => builder.replace( i, i+1, "&#x2F;" )
      case _ => } ; i -= 1 }
    builder.result }
}
