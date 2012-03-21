package ch.epfl.insynth.library

import scala.tools.nsc.interactive.Global

import scala.tools.nsc.util.SourceFile

import ch.epfl.insynth.library.loader._
import ch.epfl.insynth.library.prover._

import java.io._
import scala.io.Source
import scala.math._

/*
 *
 * Main iSynth class.
 * Synthesizes an expression at a given position.
 *
 */
class ISynth(protected val compiler: Global) extends Loaders with Provers with ISynthConfigs {

 import compiler._

 private var predefsLoaded = false;
 
 private final val CONFIG_PATH = "insynth_config.xml"

  /*
   *
   * Responsible for loading symbols and turning them into definitions.
   *
   */
  private val loader = new Loader()

  /*
   *
   * Keeps configurations.
   *
   */
  private val iConfig = new ISynthConfig(CONFIG_PATH)

  /*
   *
   * Keeps a list of all forbidden symbols.
   *
   */
  //val testFilter = new TestFilter(projecRoot, iConfig)

  /*
   *
   * Loads predef symbols to the loader.
   *
   */
  def loadPredefs(){
   this.loader.loadPredefs(definitions.PredefModule, definitions.ScalaPackage, definitions.JavaLangPackage)

    loadExamples()
  }

  var texamples = Map[String, Set[String]]() 

  

  
  def loadExamples(){    
    val lines = Source.fromInputStream(this.getClass().getResourceAsStream("/resources/Examples.txt")).getLines
    //val lines = Source.fromFile("resources\\Examples.txt").getLines //"C:\\Users\\gvero\\Examples.txt").getLines

    lines.foreach{
      x =>  
    val elem = x.split(";")
	examples += elem(0) -> (215. + (785./(Integer.parseInt(elem(1))+1)))
    }

    //}
  }

  /*
   *
   * Synthesizes an expression at a given position.
   *
   */
  def getAPISuggestionAt(pos: Position): List[String] = {
    try {

      if (!predefsLoaded){
        loadPredefs()
        predefsLoaded = true
      }
      
      val context = this.loadAllSymbols(pos)

      val proverOutput = callProver(context)

      filter(pos, context.getTopClass, proverOutput)
   } catch {
      case e:Exception =>
	List.empty[String]
    } finally {
    }
  }

  /*
   *
   * Use the loader to load all visible symbols at a given position.
   *
   */
  private def loadAllSymbols(pos:Position):ISynthContext = {
    var tree = wrapTypedTree(pos.source, false)
    println(tree)
    this.loader.load(pos, tree) //typedTree(pos.source, false))    
  }

  var exampleWeigthTime = 0
  var exampleLoadTime = 0

  private def buildMap(context:ISynthContext)={
    var map = Map("Arrow"-> 1., "Var"-> 2., context.getQuery.abstName -> 1.)

    val types = context.getTypeSystem.abstGrounds
    
    types.foreach {
      x => map += x -> 2.
    }

    if (iConfig.isVicinity_ON) {

      var localWeight = 0.
      context.getLocals.foreach {
      x => 
	if (localWeight < 5.) localWeight+=1.
	map += (x.abstName -> localWeight)
      }

      context.getClassDefs.foreach {
	x => map += (x.abstName -> 10.)
      }

      //TODO:Make this weight assigment more refined, maybe we should split the nested classes in groups based on the level they are collected at 
      context.getNestedDefs.foreach {
	x => map += (x.abstName -> 8.)
      }

      context.getPackageDefs.foreach {
	x => map += (x.abstName -> 15.)
      }

      context.getUserDefs.foreach {
	x => map += (x.abstName -> 400.)
      }

      context.getInheritanceDefs.foreach {
	x => map += (x.abstName -> 4000.)
      }    

    } else {
      context.getLocals.foreach {
	x => map += (x.abstName -> 5.)
      }

      context.getClassDefs.foreach {
	x => map += (x.abstName -> 5.)
      }

      context.getPackageDefs.foreach {
	x => map += (x.abstName -> 5.)
      }

      context.getNestedDefs.foreach {
	x => map += (x.abstName -> 5.)
      }

      context.getUserDefs.foreach {
	x => map += (x.abstName -> 5.)
      }    

      context.getInheritanceDefs.foreach {
	x => map += (x.abstName -> 5.)
      }    
    }
    val desiredType = context.getQuery.realType.typeSymbol.fullName

    val factorSet = 
      if(iConfig.isDesiredTypeSet_ON && texamples.contains(desiredType))
	texamples(desiredType)
      else 
	Set[String]()

    context.getImportedDefs.foreach {
      x =>
	if (iConfig.isVicinity_ON){
	  if (iConfig.isFrequencies_ON) {
	    if(examples.contains(x.realName)) {
	      map += (x.abstName -> (examples(x.realName) - factor(x.realName, factorSet)))
	    } else {
	      map += (x.abstName -> 1000.)
	    }
          } else {
	    map += (x.abstName -> 1000.)
	  }
	} else {
	    map += (x.abstName -> 5.)
	} 
    }

    context.getPredefs.foreach {
      x =>
	if (iConfig.isVicinity_ON){
	  if (iConfig.isFrequencies_ON){
	    if (examples.contains(x.realName)) {
	      map += (x.abstName -> (examples(x.realName) - factor(x.realName, factorSet)))
	    } else {
	      map += (x.abstName -> 1000.)
	    }
	  } else {
	    map += (x.abstName -> 1000.)
	  }
	} else {
	    map += (x.abstName -> 5.)
	}
    }


    context.getSubtypeDefs.foreach {
      x =>
	if (iConfig.isVicinity_ON){
	  if (iConfig.isFrequencies_ON){
	    if (examples.contains(x.realName)) {
	      map += (x.abstName -> (3000 + examples(x.realName) - factor(x.realName, factorSet)))
	    } else {
	      map += (x.abstName -> 4000.)
	    }
	  } else {
	    map += (x.abstName -> 4000.)
	  }
	} else {
	    map += (x.abstName -> 5.)
	}
    }

    map
  }

  def factor(name:String, factorSet:Set[String]):Double = if (factorSet.contains(name)) 200. else 0.

  //var factorSet = Set("method java.io.FileInputStream.<init> java.io.File", "method java.io.File.<init> java.lang.String")

  //example map
  var examples = Map[String, Double]()

  /*
   *
   * Sets and runs a prover.
   *
   */
  private def callProver(context:ISynthContext):ProverOutput = {
    val fmap = buildMap(context)
    val clausesNoTau = Prover.removeTau(context.getNonQueryDefs.map(_.clause))
    val realGroundMap = context.getTypeSystem.realGroundMap

    var out = new BufferedWriter(new FileWriter("C:\\Users\\gvero\\out1.txt"));
    
    clausesNoTau.foreach{
      x => out.write(x.definition.realName+"\n")
    }
    
    val prover = new Prover(clausesNoTau, context.getQuery.clause, new SubtypingUnifier(), this.iConfig.getMaxSnippets, this.iConfig.getTimeout, x => x*5, fmap, Nil, realGroundMap, iConfig.isBackwardSearch_ON)

    out.write("Solutions\n")
    
    val proverOutput = prover.run()
    
    proverOutput.solutions.foreach{
      x => out.write(x+"\n")
    }
    
    out.flush
    out.close 
    
    proverOutput
  }

  /*
   *
   * If test filter flag is on, filters out all expression that fail tests in the project.
   *
   */
  def wrapReloadPosition(p: Position): Unit =
    wrapReloadSource(p.source)

  def wrapReloadSource(source: SourceFile): Unit =
    wrapReloadSources(List(source))

  def wrapReloadSources(sources: List[SourceFile]): Unit = {
    /*val superseeded = scheduler.dequeueAll {
      case ri: ReloadItem if ri.sources == sources => Some(ri)
      case _ => None
    }
    superseeded.foreach(_.response.set())
    */
    wrap[Unit](r => new ReloadItem(sources, r).apply(), _ => ())
  }
  
  
  
  private def filter(pos:Position, mainClass:ClassInfo, output:ProverOutput):List[String] = {
    output.solutions
  }


  private def wrapTypedTree(source: SourceFile, forceReload: Boolean): Tree =
    wrap[Tree](r => new AskTypeItem(source, forceReload, r).apply(), t => throw t)


  private def wrap[A](compute: Response[A] => Unit, handle: Throwable => A): A = {
    val result = new Response[A]
    compute(result)
    result.get.fold(o => o, handle)
  }
  
  

}

