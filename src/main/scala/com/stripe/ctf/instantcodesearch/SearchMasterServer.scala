package com.stripe.ctf.instantcodesearch

import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpResponseStatus}
import org.jboss.netty.util.CharsetUtil.UTF_8
import scala.util.parsing.json._

class SearchMasterServer(port: Int, id: Int) extends AbstractSearchServer(port, id) {
  val NumNodes = 3

  def this(port: Int) { this(port, 0) }

  val clients = (1 to NumNodes)
    .map { id => new SearchServerClient(port + id, id)}
    .toArray

  override def isIndexed() = {
    val responsesF = Future.collect(clients.map {client => client.isIndexed()})
    val successF = responsesF.map {responses => responses.forall { response =>

        (response.getStatus() == HttpResponseStatus.OK
          && response.getContent.toString(UTF_8).contains("true"))
      }
    }
    successF.map {success =>
      if (success) {
        successResponse()
      } else {
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "Nodes are not indexed")
      }
    }.rescue {
      case ex: Exception => Future.value(
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "Nodes are not indexed")
      )
    }
  }

  override def healthcheck() = {
    val responsesF = Future.collect(clients.map {client => client.healthcheck()})
    val successF = responsesF.map {responses => responses.forall { response =>
        response.getStatus() == HttpResponseStatus.OK
      }
    }
    successF.map {success =>
      if (success) {
        successResponse()
      } else {
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "All nodes are not up")
      }
    }.rescue {
      case ex: Exception => Future.value(
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "All nodes are not up")
      )
    }
  }

  override def index(path: String) = {
    System.err.println(
      "[master] Requesting " + NumNodes + " nodes to index path: " + path
    )

    val responses = Future.collect(clients.map {client => client.index(path)})
    responses.map {_ => successResponse()}
  }

  override def query(q: String) = {
    val responses = clients.map {client => client.query(q)}
		val results = List.fromArray(responses.map {response => result(response)})
		Future.value(querySuccessResponse(results.flatten))
  }
  
  def result(response: Future[HttpResponse]): List[Match] = {
	  val buffer = response.get().getContent()
	  val strBuffer = new StringBuilder
	  
  	while(buffer.readable()) {
  		strBuffer.append( buffer.readByte.asInstanceOf[Char] )
  	}
  	val json = JSON.parseFull(strBuffer.toString())
  	json.get.asInstanceOf[Map[String, Any]]
  	val map:Map[String,Any] = json.get.asInstanceOf[Map[String, Any]]
  	val matches:List[String] = map.get("results").get.asInstanceOf[List[String]]
		matches.map {matchString => new Match(matchString.split(":")(0), matchString.split(":")(1).toInt)}
  }
}
