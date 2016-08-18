package cipetpet.jsonwadl

import java.lang.annotation.Annotation
import java.lang.reflect.Parameter
import java.util.{Set, ArrayList, HashMap}
import javax.ws.rs.core.{MediaType, UriBuilder}
import javax.ws.rs.{GET, Path, Produces, PathParam, QueryParam}

import com.fasterxml.jackson.annotation.{JsonAutoDetect, PropertyAccessor}
import com.fasterxml.jackson.databind.{ObjectMapper}

@Path("/jsonwadl/") 
class JsonWadl extends Object {

  class RestParam(val name: String, var ptype: String)
  class DataParam(val name: String, var data: Any)
  class RestResource(var path: String, var method: String,  var produces: String, var returns: String, 
                     var pathParams: Array[RestParam], var queryParams: Array[RestParam], var dataParam: String) {
    def this() = this("", "", "", "", Array(), Array(), "")
    def pathS (p: String) {
      path = p.replaceAll("//", "/")
    }
  }
  class RestResources(var resources: Array[RestResource], var dataParams: HashMap[String, Any]) {
    def this() = this(Array(), new HashMap())
  }

  var clazz: Class[Object] = classOf[Object]
  var restResources = new RestResources
  var mapper = new ObjectMapper
  mapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)

  def this(c: Class[Object]) {
    this()
    this.clazz = c
  }

  @Path("/") @GET @Produces(Array(MediaType.TEXT_PLAIN))
  def jsonwadl: String = {
    restResources = new RestResources
    try {
      var methods = clazz.getDeclaredMethods
      var basePath = clazz.getAnnotation(classOf[Path]).value
      for (method <- methods if method.getAnnotation(classOf[Path])!=null) {
        var restResource = new RestResource
        restResources.resources +:= restResource
        parseMethodAnnotations(basePath, restResource, method.getAnnotations)
        parseParameterAnnotations(restResource, method.getParameters)
        restResource.returns = method.getReturnType().getSimpleName
        addDataObject(method.getReturnType)
      }
      return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(restResources)
    } catch {
        case e: Exception => {
          System.err.println(e.printStackTrace)
          return e.getMessage
        }
    }
  }

  def parseParameterAnnotations(restResource: RestResource, ps: Array[Parameter]) {
    for (param <- ps) {
      var paramAnnos = param.getAnnotations
      for (annon <- paramAnnos) {
        var name = param.getType().getSimpleName;
        annon.annotationType().getSimpleName match {
          case "PathParam" => 
            restResource.pathParams +:= new RestParam(annon.asInstanceOf[PathParam].value, name)
          case "QueryParam" => 
            restResource.queryParams +:= new RestParam(annon.asInstanceOf[QueryParam].value, name)
          case _ => ""
        }
      }
      if (paramAnnos.length==0) { // Data post stuff
        restResource.dataParam = param.getType().getSimpleName
        addDataObject(param.getType)
      }
    }
  }

  def parseMethodAnnotations(basePath: String, restResource: RestResource, methodAnnons: Array[Annotation]) {
    for (annon <- methodAnnons) {
      annon.annotationType().getSimpleName match {
        case "Path" => restResource.pathS(basePath + annon.asInstanceOf[Path].value)
        case "GET" => restResource.method = "GET"
        case "POST" => restResource.method = "POST"
        case "PUT" => restResource.method = "PUT"
        case "DELETE" => restResource.method = "DELETE"
        case "PATCH" => restResource.method = "PATCH"
        case "Produces" => restResource.produces = annon.asInstanceOf[Produces].value.asInstanceOf[Array[String]](0)
        case _ => ""
      }
    }
  }

  def addDataObject(clazz: Class[_]) : Unit = {
    var key = clazz.getSimpleName
    var ob = clazz.newInstance
    if(clazz.getCanonicalName().startsWith("java")) return
    restResources.dataParams.put(key, ob)
  }

}
