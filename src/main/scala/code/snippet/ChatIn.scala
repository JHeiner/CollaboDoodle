
package code.snippet

import net.liftweb._
import http._
import js._
import JsCmds._
import JE._

object ChatIn
{
  def render = SHtml.onSubmit( s => {
	code.comet.ChatServer ! s
	SetValById("chat_in","") })
}
