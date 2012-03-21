package ch.epfl.insynth.library

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.library.definitions._

/*
 *
 * Main iSynth class.
 * Synthesizes an expression at a given position.
 *
 */
trait Contexts extends Definitions {
 self: ISynth =>

  /*
   *
   * Holdes definitions together with their type and naming systems.
   *
   */ 
  class ISynthContext(
    private var typeSystem:CompositeTypeSystem,
    private var renamer:Renamer[String, String],
    private var predefs:List[ClauseDefinition]
  ) extends Cloneable {
    private var query:QueryInfo = null
    private var field:ClauseDefinition = null
    private var clazz:ClassInfo = null
    private var method:ClauseDefinition = null
    private var locals:List[LocalInfo] = List[LocalInfo]()
    private var classDefs:List[ClauseDefinition] = List[ClauseDefinition]()
    private var packageDefs:List[ClauseDefinition] = List[ClauseDefinition]()
    private var importedDefs:List[ClauseDefinition] = List[ClauseDefinition]()

    private var nestedDefs:List[ClauseDefinition] = List[ClauseDefinition]()
    private var subtypeDefs:List[ClauseDefinition] = List[ClauseDefinition]()

    private var inheritanceDefs:List[ClauseDefinition] = List[ClauseDefinition]()
    private var userDefs:List[ClauseDefinition] = List[ClauseDefinition]()

    def this(renamer:Renamer[String,String]) = this(new CompositeTypeSystem(), renamer, List[ClauseDefinition]())

    def getUserDefs = this.userDefs
    def setUserDefs(userDefs:List[ClauseDefinition]){this.userDefs = userDefs}

    def getNestedDefs = this.nestedDefs
    def setNestedDefs(nestedDefs:List[ClauseDefinition]){this.nestedDefs = nestedDefs}

    def getTopClass = this.clazz
    def setTopClass(clazz:ClassInfo){this.clazz = clazz}
    
    def getField = this.field
    def setField(filed:ClauseDefinition){this.field = field}
    
    def getMethod = this.method
    def setMethod(method:ClauseDefinition){this.method = method}

    def getTypeSystem = this.typeSystem
    def setTypeSystem(typeSystem:CompositeTypeSystem){this.typeSystem = typeSystem}
    
    def getRenamer = this.renamer
    def setRenamer(renamer:Renamer[String, String]){this.renamer = renamer}
    
    def getPredefs = this.predefs
    def setPredefs(predefs:List[ClauseDefinition]){this.predefs = predefs}
       
    def getQuery = this.query
    def setQuery(query:QueryInfo){this.query = query}

    def getLocals = this.locals
    def setLocals(locals:List[LocalInfo]){this.locals = locals}
    
    def getClassDefs = this.classDefs
    def setClassDefs(classDefs:List[ClauseDefinition]){this.classDefs = classDefs}
    
    def getPackageDefs = this.packageDefs
    def setPackageDefs(packageDefs:List[ClauseDefinition]){this.packageDefs = packageDefs}
    
    def getImportedDefs = this.importedDefs
    def setImportedDefs(importedDefs:List[ClauseDefinition]){this.importedDefs = importedDefs}
    
    def getInheritanceDefs = this.inheritanceDefs
    def setInheritanceDefs(inheritanceDefs:List[ClauseDefinition]){this.inheritanceDefs = inheritanceDefs}
    
    def getSubtypeDefs = this.subtypeDefs
    def setSubtypeDefs(subtypeDefs:List[ClauseDefinition]){this.subtypeDefs = subtypeDefs}

    def getDefinitions() = (if (this.query != null) List[ClauseDefinition](this.query) else List[ClauseDefinition]()) ++ this.getNonQueryDefs

    def getNonQueryDefs = this.locals ++ this.classDefs ++ this.packageDefs ++ this.importedDefs ++ this.userDefs ++ this.predefs ++ this.nestedDefs ++ this.subtypeDefs ++ this.inheritanceDefs

    override def toString = getDefinitions().mkString("\n")
  }

  class PreDefHolder(typeSystem:CompositeTypeSystem, renamer:Renamer[String, String], preDefs:List[ClauseDefinition]){
    def createISynthContext():ISynthContext = new ISynthContext(typeSystem.clone(), renamer.clone(), preDefs)
  }
}
















