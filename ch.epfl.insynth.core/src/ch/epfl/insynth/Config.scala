package ch.epfl.insynth

import java.util.logging._

object Config {
  final val getTimeOutSlot = 500
  
  final val loadPredefs = true
  
  final val numberOfSnippets = 10
  
  val logCombinatorInputProofTreeLevel = 6
  
  //final val outputFileName = "C:\\Users\\gvero\\insynthOutput.txt"
  //final val errorFileName = "C:\\Users\\gvero\\insynthError.txt"
    
  //final val debugFileName = "C:\\Users\\gvero\\insynthDebug.txt"
    
  // logging for code generation phase
    
  // defines loggers
  val logger = Logger.getLogger("reconstruction.combination")
  val logStructures = Logger.getLogger("reconstruction.combination.structures")
  val logApply = Logger.getLogger("reconstruction.combination.apply")  
  val logPQAdding = Logger.getLogger("reconstruction.combination.apply.pqadding")
  
  val logExtractor = Logger.getLogger("reconstruction.extractor")
  
  val isLogging = false
  
  // static code for loggers setup
  {  
    val array = Array(logger, logStructures, logApply, logExtractor, logPQAdding)
    
    // remove all handlers
    for (logger <- array)
    for (handler <- logger.getHandlers)
      logger.removeHandler(handler)
      
    Logger.getLogger("reconstruction.combination").setLevel(Level.FINEST)
    Logger.getLogger("reconstruction.combination.apply").setLevel(Level.FINEST)
    Logger.getLogger("reconstruction.combination.structures").setLevel(Level.INFO)
    logPQAdding.setLevel(Level.FINEST)
    
    logExtractor.setLevel(Level.FINEST)
      
    val handler = new FileHandler("%h/combinator%u.log");
    val handlerEx = new FileHandler("%h/extractor%u.log");
    handler.setFormatter(new SimpleFormatter)
    handlerEx.setFormatter(new SimpleFormatter)
    // PUBLISH this level
    handler.setLevel(Level.FINEST);
    logApply.addHandler(handler);
    logStructures.addHandler(handler);
    logExtractor.addHandler(handlerEx)
//    Logger.getLogger("reconstruction.combination.apply").addHandler(handler);
//    Logger.getLogger("reconstruction.combination.structures").addHandler(handler);
  }
}