package ch.epfl.insynth.library.types

import scala.tools.nsc.interactive.Global

import ch.epfl.insynth.library.renamers._
import ch.epfl.insynth.library.ISynth

trait Types extends Renamers {
  self: ISynth =>

  trait TypeSystem[K >: Null,V >: Null] {
    
    def abstVariable(name:K):V
    def abstConstant(name:K):V 
    def abstInstance(name:K):V 
    
    def realVariable(name:V):K
    def realConstant(name:V):K  
    def realInstance(name:V):K 

  }

  trait TypeList[K >: Null,V >: Null] {

    def abstVariables:List[V]
    def abstConstants:List[V]
    def abstInstances:List[V]
    def abstGroundVariables:List[V]

    def realVariables:List[K]
    def realConstants:List[K]
    def realInstances:List[K]
    def realGroundVariables:List[K]

    //Variables
    def abstPolys:List[V]

    //Const, Instances, and "grounded" Variables
    def abstGrounds:List[V]

    //Variables
    def realPolys:List[K]

    //Const, Instances, and "grounded" Variables
    def realGrounds:List[K]
  }

  object CompositeTypeSystem {
    final val VAR_BASE_NAME = "X"
    final val GR_VAR_BASE_NAME = "XG"
    final val CONST_BASE_NAME = "C"
    final val INST_BASE_NAME = "I"
  }

  /*
   *
   * Holds types of non-query definitions.
   *
   */
  class CompositeTypeSystem extends TypeSystem[String, String] with TypeList[String, String] with Cloneable {
    private var groundedVariables = new SimpleRenamer[String,String](new SimpleNameGenerator(CompositeTypeSystem.GR_VAR_BASE_NAME))
    private var variables = new CompositeRenamer[String,String]()
    private var constants = new SimpleRenamer[String,String](new SimpleNameGenerator(CompositeTypeSystem.CONST_BASE_NAME))
    private var instances = new SimpleRenamer[String,String](new SimpleNameGenerator(CompositeTypeSystem.INST_BASE_NAME))

    private val variableBaseNameGenerator = new SimpleNameGenerator(CompositeTypeSystem.VAR_BASE_NAME)

    private var subtypes = Map[String,List[String]]()

    def hasSubtypes(name:String) = subtypes.contains(name)

    def addSubtypes(name:String, subs:List[String]){ subtypes += name -> subs }

    def getSubtypes = subtypes

    def pushVariableRenamer(){this.variables.addRenamer(new SimpleRenamer[String,String](new SimpleNameGenerator(variableBaseNameGenerator.generate())))}

    def pushVariableRenamer(vars:List[String]){
      this.variables.addRenamer(new SimpleRenamer[String,String](new SimpleNameGenerator(variableBaseNameGenerator.generate())))
      vars.foreach(this.abstVariable(_))
    }

    def popVariableRenamer(){
      this.variables.removeRenamer()
      this.variableBaseNameGenerator.decrement()
    }

    def topVariableRenamer = this.variables.topRenamer

    def abstVariable(name:String):String = this.variables.abstName(name)
    def abstConstant(name:String):String = this.constants.abstName(name)
    def abstInstance(name:String):String = this.instances.abstName(name)
    def abstGroundVariable(name:String):String = this.groundedVariables.abstName(name)

    def realVariable(name:String):String = this.variables.realName(name)
    def realConstant(name:String):String = this.constants.realName(name)
    def realInstance(name:String):String = this.instances.realName(name)
    def realGroundVariable(name:String):String = this.groundedVariables.realName(name)

    def abstVariables:List[String] = this.variables.getMap.values.toList
    def abstConstants:List[String] = this.constants.getMap.values.toList
    def abstInstances:List[String] = this.instances.getMap.values.toList
    def abstGroundVariables:List[String] = this.groundedVariables.getMap.values.toList

    def realVariables:List[String] = this.variables.getReverseMap.values.toList
    def realConstants:List[String] = this.constants.getReverseMap.values.toList
    def realInstances:List[String] = this.instances.getReverseMap.values.toList
    def realGroundVariables:List[String] = this.groundedVariables.getReverseMap.values.toList

    //Variables
    def abstPolys:List[String] = this.abstVariables

    //Const, Instances, and "grounded" Variables
    def abstGrounds:List[String] = this.abstConstants ++ this.abstInstances ++ this.abstGroundVariables

    //Variables
    def realPolys:List[String] = this.realVariables

    //Const, Instances, and "grounded" Variables
    def realGrounds:List[String] = this.realConstants ++ this.realInstances ++ this.realGroundVariables

    def realGroundMap = {
      var map = Map[String, String]()

      map++= this.constants.getReverseMap
      map++= this.instances.getReverseMap
      map++= this.groundedVariables.getReverseMap

      map
    }

    override def clone():CompositeTypeSystem = {
      val typeSystem = super.clone().asInstanceOf[CompositeTypeSystem]
      typeSystem.groundedVariables = this.groundedVariables.clone()
      typeSystem.variables = this.variables.clone()
      typeSystem.constants = this.constants.clone()
      typeSystem.instances = this.instances.clone()
      typeSystem
    }
  }
}
