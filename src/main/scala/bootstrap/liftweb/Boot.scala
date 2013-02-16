package bootstrap.liftweb

import net.liftweb.http.{Html5Properties,LiftRules,Req}
import net.liftweb.sitemap.{Menu,SiteMap}
import net.liftweb.common.Full

import com.mongodb.{ServerAddress,Mongo}
import net.liftweb.mongodb.{MongoDB,DefaultMongoIdentifier}

class Boot
{
  def boot { Boot() }
}
object Boot
{
  def apply() {
	LiftRules.addToPackages("code")
	LiftRules.setSiteMap(siteMap)
 //Show the spinny image when an Ajax call starts
 LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
 // Make the spinny image go away when it ends
 LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
 LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
	LiftRules.htmlProperties.default.set(html5 _)
	mongo
  }

  def html5(r:Req) = new Html5Properties(r.userAgent)

  def mdb = new Mongo(new ServerAddress("127.0.0.1",27017))
  def mongo = MongoDB.defineDb(DefaultMongoIdentifier,mdb,"mydb")

  def siteMap = SiteMap(
	Menu.i("Home") / "index"
  )
}
