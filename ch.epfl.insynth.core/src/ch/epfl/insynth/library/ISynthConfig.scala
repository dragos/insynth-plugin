package ch.epfl.insynth.library

import scala.xml.{XML, Node}
import java.io._


trait ISynthConfigs {
  class ISynthConfig(path:String) {
    private var testFilter = false
    private var sbtroot = ""
    private var timeout:Long = 1000
    private var maxsnippets:Int = 2
    private var desiredTypeSet_ON = true
    private var frequencies_ON = true
    private var vicinity_ON = true
    private var backwardSearch_ON = true

    loadConfig()

    def testFilterOn = testFilter
    def getMaxSnippets = maxsnippets
    def getTimeout = timeout
    def getSbtRoot = sbtroot
    def isDesiredTypeSet_ON = desiredTypeSet_ON
    def isFrequencies_ON = frequencies_ON
    def isVicinity_ON = vicinity_ON
    def isBackwardSearch_ON = backwardSearch_ON

    private def loadConfig(){

      try {
	//val xml = XML.load(this.getClass().getResourceAsStream("/"+path))
       val xml = XML.load(getClass().getResourceAsStream("/resources/config.xml")) // "resources\\config.xml")//"C:\\Users\\gvero\\config.xml")

	xml match {
	  case <config/> =>
	    var prop = xml \ "@testfilter"
	  testFilter = if (prop != null) {
	    prop.text match {
	      case "true" => true
	      case _ => false
	    }
	  }
	  else 
	    false

	  prop = xml \ "@sbtroot"
	  sbtroot = if (prop != null){
	    prop.text
	  }else {
	    testFilter = false
	    ""
	  }

	  prop = xml \ "@timeout"
	  timeout = if (prop != null)
	    try{
	      Integer.parseInt(prop.text)
	    } catch {
	      case _ => 1000
	    }
		    else 
		      1000

	  prop = xml \ "@maxsnippets"
	  maxsnippets = if (prop != null)
	    try{
	      Integer.parseInt(prop.text)
	    } catch {
	      case _ => 2
	    }
	    else 
	     2

	  prop = xml \ "@desiredTypeSet"
	  desiredTypeSet_ON = if (prop != null) {
	    prop.text match {
	      case "true" => true
	      case _ => false
	    }
	  }
	  else 
	    true

	  prop = xml \ "@frequencies"
	  frequencies_ON = if (prop != null) {
	    prop.text match {
	      case "true" => true
	      case _ => false
	    }
	  }
	  else 
	    true

	  prop = xml \ "@vicinity"
	  vicinity_ON = if (prop != null) {
	    prop.text match {
	      case "true" => true
	      case _ => false
	    }
	  }
	  else 
	    true

	  prop = xml \ "@backwardSearch"
	  backwardSearch_ON = if (prop != null) {
	    prop.text match {
	      case "true" => true
	      case _ => false
	    }
	  }
	  else 
	    true

	  case _ =>
	}
      } catch {
	    case e:Exception => {
	      println("NO RESOURCE found!")
	      println(e.getMessage)
	   }
      }
    }
  }
}
