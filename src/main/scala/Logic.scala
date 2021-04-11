import scala.concurrent.duration.DurationInt

import monix.eval.Task
import org.jsoup.{HttpStatusException, Jsoup}
import scala.jdk.CollectionConverters._


object Logic {
  case class WikiLink(title: String, url: String)

  case class WikiPage(
    link: WikiLink,
    code: Int,
    error: Option[String],
    children: List[WikiLink],
  )

  def fetchPage(url: String): Task[WikiPage] = Task {
    val boringPrefixes = Set("File:", "Wikipedia:", "Special:")
    try {
      val doc = Jsoup.connect(url).get
      val children = doc.select("#mw-content-text a[href^='/wiki']").asScala.toList
        .map(e => WikiLink(e.attr("title"), e.absUrl("href")))
        .filterNot(c => boringPrefixes.exists(c.url.contains))
        .sortBy(_.title)
        .distinctBy(_.url.takeWhile(_ != '#'))

      WikiPage(
        WikiLink(doc.title(), url),
        200,
        None,
        children
      )
    } catch {
      case e: HttpStatusException =>
        WikiPage(WikiLink("Error", url), e.getStatusCode, Some(e.getMessage), Nil)
    }
  }.timeout(5.seconds)
}
