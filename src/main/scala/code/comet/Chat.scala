
package code.comet

import scala.xml.{Elem,NodeSeq,Text}
import scala.collection.mutable
import net.liftweb.common.{Box,Full,Empty}
import net.liftweb.http.{S,SHtml,CometListener,ListenerManager,RenderOut,Req}
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
object Message
{
  val Empty = Message( null, null, null, null )

  def user( id:String, nick:String, text:String ) = {
	val trimmed = text.trim // empty user messages get dropped
	if ( trimmed.isEmpty ) Message.Empty else
	  // user messages never have a command
      Message( id, "", nick, trimmed ) }

  def admin( command:String, id:String, info:String ) =
	// admin user is "0", messages always have a command,
	// and the "nick" is reused to be the id of the instigating user
    Message( "0", command, id, info )

  val dateTimeRFC822 = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  val dateTimeFormat = new java.text.SimpleDateFormat( dateTimeRFC822 )
  def time( id:String ) =
	admin( "/time", id, dateTimeFormat.format( new java.util.Date ) )

  val serverAddresses = {
    import java.net.NetworkInterface._
    import scala.collection.JavaConversions._
    val (n4,n6) = ( for {
      i <- getNetworkInterfaces
      if ! i.isLoopback
      a <- i.getInetAddresses }
      yield a )
		.toList.partition{ _.isSiteLocalAddress }
    val ns = ( if ( n4.nonEmpty ) n4 else n6 ).map{ _.getHostAddress }
	ns.mkString( ", " ) }
  def trace( id:String ) =
	admin( "/trace", id, serverAddresses )
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
object History
{
  val empty = History( Nil )
  val initial = empty + Message.time("") + Message.trace("")
}

class Chat
extends StatefulComet
with net.liftweb.common.Logger
{
  type Delta = Message
  type State = History
  def emptyState = History.empty
  def testState( in:Any ):Box[State] = in match {
    case g:History => Full( g )
    case _ => Empty }

  override def sendInitialReq_? = true
  var remoteAddress = Option.empty[String];
  def superuser = remoteAddress.exists( _ == "127.0.0.1" )
  override def captureInitialReq( req:Box[Req] ) { req.map { r =>
	remoteAddress = Option( r.request ).map{ _.remoteAddress } }}

  var user = "" ; var nick = "" ; var allowed = true

  def userMessage( text:String ) = Message.user( user, nick, text )
  def timeMessage = Message.time( user )
  def traceMessage = Message.trace( user )
  def adminMessage( command:String, info:String ) =
	Message.admin( command, user, info )

  def sendChars( dangerous:String ) { sendInput( false, dangerous ) }
  def sendDoodle( dangerous:String ) { sendInput( true, dangerous ) }
  def sendInput( doodle:Boolean, dangerous:String ) {
	if ( ! allowed ) return
	val trimmed = dangerous.trim
	if ( trimmed.nonEmpty )
	  ChatServer ! Input( this, doodle, trimmed ) }

  override def render:RenderOut = Seq(
    SHtml.ajaxForm( SHtml.text( "", sendChars, "id"->"ChatInput" ),
                    JsCmds.Noop, JE.JsRaw( "ChatInput.value=''" ) ),
    SHtml.ajaxForm( SHtml.hidden( sendDoodle _, "", "id"->"ChatHidden" ),
                    JsCmds.Noop, JE.JsRaw( "ChatCheck()" ) ),
    JsCmds.Script( state.toJs( user ) ) )

  ChatServer ! Add( this )
}

case class Add( chat:Chat )
case class Remove( chat:Chat )

case class Input( chat:Chat, doodle:Boolean, dangerous:String )
{
  def parse:Message = {

	// dangerous was already trimmed by Chat.sendInput,
	// assume escaping doesn't add whitespace to either end...
    val builder = new StringBuilder( dangerous.length )
    AltXML.toXML( Text(dangerous), null, builder, false, false )
    var i = builder.length - 1
    while ( i >= 0 ) { builder.charAt( i ) match {
      // see owasp.org XSS prevention cheat sheet...
      case '\'' => builder.replace( i, i+1, "&#x27;" )
      case '/' => builder.replace( i, i+1, "&#x2F;" )
      case _ => } ; i -= 1 }

	if ( doodle )
	  return chat.userMessage( builder.result )

	val front = builder.substring( 0, builder.length min 12 ).toLowerCase

	if ( front.startsWith( "doodle:" ) ) {
	  builder.insert( 6, ' ' ) // foil attempts to type doodles
	  return chat.userMessage( builder.result ) }

    if ( front == "&#x2f;time" )
      return chat.timeMessage

	if ( front == "&#x2f;trace" )
	  return chat.traceMessage

    if ( front.startsWith( "&#x2f;nick" )
		&& ( builder.length == 10 || builder.charAt( 10 ).isWhitespace ) ) {
      val oldnick = chat.nick ; chat.nick = builder.substring( 10 ).trim
      val into = if ( chat.nick == oldnick ) "" else (" \u27a1 "+chat.nick)
      return chat.adminMessage( "/nick", oldnick+into ) }

	if ( front.startsWith( "&#x2f;kick" ) && chat.superuser
		&& builder.length > 10 && builder.charAt( 10 ).isWhitespace ) {
	  val kick = ChatServer.parseKick( chat, builder.substring( 10 ).trim )
	  if ( kick.nonEmpty ) return kick.get }

	return chat.userMessage( builder.result ) }
}

object ChatServer
extends LiftActor
{
  private var history = History.initial
  private var userCounter = 0
  private val listenerMap = new mutable.HashMap[String,Chat]

  protected def messageHandler = {
	case Add( chat ) =>
	  userCounter += 1
	  chat.user = userCounter.toString
	  listenerMap( chat.user ) = chat
	  chat ! history
	case Remove( chat ) =>
	  listenerMap.remove( chat.user )
    case input @ Input( chat, _, _ ) =>
	  if ( chat.allowed ) {
	  val message = input.parse
	  if ( message != Message.Empty ) {
	  history += message
	  for ( chat <- listenerMap.values ) chat ! history } } }

  def parseKick( admin:Chat, who:String ):Option[Message] =
	for ( baddie <- listenerMap.get( who ) ; if baddie != admin )
	yield {
	  val message = admin.adminMessage( "/kick", baddie.user+" "+baddie.nick )
	  baddie.allowed = false
	  baddie ! ( history + message
		 + Message("","","","you have been kicked off the server" ) )
	  listenerMap.remove( baddie.user )
	  message }
}
