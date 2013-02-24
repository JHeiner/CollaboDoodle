
package code.snippet

object ChatClient
extends net.liftweb.http.NamedCometActorSnippet
{
  def cometClass = "Chat"
  def name = net.liftweb.util.Helpers.nextFuncName
}
