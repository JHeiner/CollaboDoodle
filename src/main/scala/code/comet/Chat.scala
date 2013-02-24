
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

case class Message( user:String, text:String, extra:String, num:Int )
extends DeltaTrait
{
  require( user.nonEmpty, "all messages must specify instigating user" )
  require( user.trim == user && text.trim == text && extra.trim == extra )

  def toJs:JsCmd =
	if ( extra.isEmpty ) JE.Call( "ChatAppend", user, text )
	else JE.Call( "ChatAppend", user, text, extra )
}
object Message
{
  def apply( user:String, text:String ):Message = apply( user, text, "" )
  def apply( user:String, text:String, extra:String ):Message = {
    val builder = new StringBuilder( text.length )
    AltXML.toXML( Text(text), null, builder, false, false )
    var i = builder.length - 1
    while ( i >= 0 ) { builder.charAt( i ) match {
      // see owasp.org XSS prevention cheat sheet...
      case '\'' => builder.replace( i, i+1, "&#x27;" )
      case '/' => builder.replace( i, i+1, "&#x2F;" )
      case _ => } ; i -= 1 }
	apply( user, builder.result, extra, -1 ) }

  val dateTimeRFC822 = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  val dateTimeFormat = new java.text.SimpleDateFormat( dateTimeRFC822 )
  def dateTimeCurrent = dateTimeFormat.format( new java.util.Date )
  def time( id:String ) = Message( id, dateTimeCurrent, "/time" )

  val serverAddresses = {
    import scala.collection.JavaConversions._
    val (n4,n6) = ( for {
      i <- java.net.NetworkInterface.getNetworkInterfaces
      if ! i.isLoopback
      a <- i.getInetAddresses }
      yield a )
		.toList.partition{ _.isSiteLocalAddress }
    val ns = ( if ( n4.nonEmpty ) n4 else n6 ).map{ _.getHostAddress }
	ns.mkString( ", " ) }
  def trace( id:String ) = Message( id, serverAddresses, "/trace" )
}

case class History( messages:List[Message] )
extends CometState[Message,History]
with net.liftweb.common.Logger
{
  def isEmpty = messages.isEmpty
  def size = if ( isEmpty ) 0 else messages.head.num

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
  def initial = empty + Message.time("0") + Message.trace("0")
}

class Chat
extends StatefulComet
with net.liftweb.http.NamedCometActorTrait
{
  type Delta = Message
  type State = History
  def emptyState = History.empty
  def testState( in:Any ):Box[State] = in match {
    case g:History => Full( g )
    case _ => Empty }
  override def lowPriority = {
	case Clear => state = emptyState }

  override def sendInitialReq_? = true
  var remoteAddress = Option.empty[String];
  def superuser = remoteAddress.exists( _ == "127.0.0.1" )
  override def captureInitialReq( req:Box[Req] ) { req.map { r =>
	remoteAddress = Option( r.request ).map{ _.remoteAddress } }}

  var user = "" ; var nick = "" ; var allowed = true

  def userMessage( text:String ) = Message( user, text )
  def timeMessage = Message.time( user )
  def traceMessage = Message.trace( user )
  def adminMessage( text:String, extra:String ) = Message( user, text, extra )

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

  override def localSetup() { ChatServer ! Add( this ) }
  override def localShutdown() { ChatServer ! Remove( this ) }
}

case class Add( chat:Chat )
case class Remove( chat:Chat )
case class Input( chat:Chat, doodle:Boolean, text:String )
case object Clear

object ChatServer
extends LiftActor
with net.liftweb.common.Logger
{
  private var history = History.empty
  private var userCounter = 0
  private val listenerMap = new mutable.HashMap[String,Chat]

  protected def messageHandler = {
	case Add( chat ) =>
	  userCounter += 1
	  chat.user = userCounter.toString
	  listenerMap( chat.user ) = chat
	  if ( history.isEmpty ) history = History.initial
	  chat ! history
	case Remove( chat ) =>
	  listenerMap.remove( chat.user )
	  if ( listenerMap.isEmpty ) history = History.empty
    case Input( chat, doodle, text ) => if ( chat.allowed ) {
	  val message = parse( chat, doodle, text )
	  if ( history.isEmpty ) {
		history = History.initial
		for ( client <- listenerMap.values ; if client.nick.nonEmpty )
		history += client.adminMessage( "\u27a1 "+client.nick, "/nick" ) }
	  history += message
	  for ( client <- listenerMap.values ) client ! history } }

  def parse( chat:Chat, doodle:Boolean, text:String ):Message =
  {
	if ( doodle )
	  return chat.userMessage( text )

	val front = text.substring( 0, text.length min 7 ).toLowerCase

	if ( front.startsWith( "doodle:" ) )
	  return chat.userMessage( "doodle :"+text.substring(7) )

    if ( front == "/time" )
      return chat.timeMessage

	if ( front == "/trace" )
	  return chat.traceMessage

	if ( front == "/clear" && chat.superuser ) {
	  history = History.empty
	  for ( client <- listenerMap.values ) client ! Clear
	  return chat.adminMessage( "", "/clear" ); }

    if ( front.startsWith( "/nick" )
		&& ( text.length == 5 || text.charAt( 5 ).isWhitespace ) ) {
      val oldnick = chat.nick ; chat.nick = text.substring( 5 ).trim
      val into = if ( chat.nick == oldnick ) "" else (" \u27a1 "+chat.nick)
      return chat.adminMessage( (oldnick+into).trim, "/nick" ) }

	if ( front.startsWith( "/kick" ) && chat.superuser
		&& text.length > 5 && text.charAt( 5 ).isWhitespace ) {
	  val kick = for {
		baddie <- listenerMap.get( text.substring( 5 ).trim )
		if baddie != chat }
	  yield {
		val message = chat.adminMessage( baddie.user+" "+baddie.nick, "/kick" )
		baddie.allowed = false
		baddie ! ( history + message
				   + Message("0","you have been kicked off the server" ) )
		listenerMap.remove( baddie.user )
		message }
	  if ( kick.nonEmpty ) return kick.get }

	return chat.userMessage( text ) }
}
