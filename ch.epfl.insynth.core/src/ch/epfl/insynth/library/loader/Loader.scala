package ch.epfl.insynth.library.loader

import ch.epfl.insynth.library.collectors._
import ch.epfl.insynth.library._

trait Loaders extends Contexts with TypeFinders with Collectors {
  self: ISynth =>
    
    import compiler._

    object Loader {
      val DEF_PREFIX = "Def"
    }

    class Loader {

      private val collector = new Collector()

      private val factory = new DefinitionFactory()

      /*
       *
       * Keeps a list of all forbidden symbols.
       *
       */
      //val blackList = new BlackList(BLACK_LIST_PATH)

      private var predefs:PreDefHolder = null

      /*
       *
       * Loads all definitions visible from a given position
       *
       */
      def load(pos:Position, tree:Tree):ISynthContext = {
	//create a new context
	val context = if (this.predefs != null) this.predefs.createISynthContext() else new ISynthContext(new SimpleRenamer[String, String](new SimpleNameGenerator(Loader.DEF_PREFIX)))

	//update the context with definitions visible from a given position
	this.updateContext(pos, tree, context)

	//filter the definitions that are forbiden by the black list
	this.filter(context)

	context
      }
      
      //Black list filter
      private def filter(context:ISynthContext){}

      def loadPredefs(predefModule:Symbol, scalaPackage:Symbol, javaLangPackage:Symbol) {
	if (this.predefs == null) {
	  val typeSystem = new CompositeTypeSystem()
	  val renamer = new SimpleRenamer[String, String](new SimpleNameGenerator(Loader.DEF_PREFIX))
	  val typeFinder = new TypeFinder(typeSystem)
	  this.predefs = new PreDefHolder(typeSystem, renamer, loadSomeImport(javaLangPackage, typeFinder, renamer)++loadSomeImport(scalaPackage, typeFinder, renamer))
	}
      }

      //TODO: see what is wrong with clazz.isModule
      //Why do we get "Cyclic self reference exception."
      private def loadImport(importSymbol:Symbol, typeFinder:TypeFinder, renamer: Renamer[String, String]):List[ClauseDefinition] = {
	var predefs = List[ClauseDefinition]()
	val imports = importSymbol.tpe.decls
	for {
	  clazz <- imports
	} try{
	  if(!clazz.nameString.contains("$")
	     && clazz.exists
	     && !clazz.isSynthetic
	     && (clazz.isClass || clazz.isModule)
	     && !clazz.isAbstractClass
	     && !clazz.isTrait
	     && !clazz.isAbstractType
	     && !clazz.isPackage)
	      predefs ++= loadClass(clazz, typeFinder, renamer)
	} catch{
	  case _ =>
	}

	predefs
      }

      private def loadSomeImport(importSymbol:Symbol, typeFinder:TypeFinder, renamer: Renamer[String, String]):List[ClauseDefinition] = {
	var predefs = List[ClauseDefinition]()
	val imports = importSymbol.tpe.decls
	for {
	  clazz <- imports
	} try{
	  if(!clazz.nameString.contains("$")
	     && (clazz.fullName.equals(scala.Array.getClass.getName.replace("$","")) || clazz.fullName.equals("java.lang.System"))
	     && clazz.exists
	     && !clazz.isSynthetic
	     && (clazz.isClass || clazz.isModule)
	     && !clazz.isAbstractClass
	     && !clazz.isTrait
	     && !clazz.isAbstractType
	     && !clazz.isPackage){
	      predefs ++= loadClass(clazz, typeFinder, renamer)
	  }
	} catch{
	  case _ =>
	}

	predefs
      }

      private def loadClass(clazz:Symbol, typeFinder:TypeFinder, renamer: Renamer[String, String]):List[ClauseDefinition] = {
	var predefs = List[ClauseDefinition]()
	val decls = clazz.tpe.decls.toList
	val clazzDef = factory.makeClass(clazz)

	for {
	  decl <- decls
	  if (!decl.nameString.contains("$") && decl.exists && ((decl.fullName.contains("scala.Array.apply")&& !clazz.isModule) || decl.fullName.contains("java.lang.System.in")) && !decl.isSynthetic && !(clazz.isModule && decl.isConstructor) && decl.isValue && !decl.isGetter && !decl.isSetter && !returnsUnit(decl))
	}{

	  if (decl.isMethod && decl.isPublic){
	    val definition = factory.makeExternalDef(decl, clazzDef)
	    if (typeFinder.setAbstType(definition)){
	      definition.setAbstName(renamer.abstName(definition.realName))
	      predefs = definition :: predefs
	    }
	  } else if (decl.isPublic || existsPublicGetter(decl, decls)){
	    val definition = factory.makeExternalDef(decl, clazzDef)
	    if (typeFinder.setAbstType(definition)){
	      definition.setAbstName(renamer.abstName(definition.realName))
	      predefs = definition :: predefs
	    }
	  }
	}
	predefs
      }

      private def updateContext(pos:Position, tree:Tree, context:ISynthContext){
	val collectedInfo = collector.collect(pos, tree)
	val typeSystem = context.getTypeSystem
	val typeFinder = new TypeFinder(typeSystem)
	
	if (loadInitialDefs(context, collectedInfo, typeFinder)){
	  
		
	  loadLocals(context, collectedInfo, typeFinder)
	  loadClass(context, collectedInfo, typeFinder)
	  loadPackage(context, collectedInfo, typeFinder)
	  loadImports(context, collectedInfo, typeFinder)
	  loadNestedClasses(context, collectedInfo, typeFinder)

	  //Last, load subclasses
	  loadSuperclasses(context, collectedInfo, typeFinder)
	  loadInheritanceDefs(context, collectedInfo, typeFinder)
	  loadUserDefs(context)
	}
      }
      
      private def loadInitialDefs(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder):Boolean = {
	if (collectedInfo.hasDesiredType && collectedInfo.hasTopClass){
	  val clazz = factory.makeClass(collectedInfo.getTopClass)
	  context.setTopClass(clazz)

	  if (collectedInfo.isMethodCompletition || collectedInfo.isLocalCompletition) {
	    val renamer = context.getRenamer
	    val methodSymbol = collectedInfo.getMethodSymbol()
	    val method = factory.makeInternalDef(methodSymbol._1, clazz, methodSymbol._2) 
	    if (typeFinder.setAbstType(method)){
	      method.setAbstName(renamer.abstName(method.realName))
	      context.setMethod(method)
	      val query = this.factory.makeQuery(collectedInfo.getDesiredType, method)
	      if (typeFinder.setQueryAndLocalAbstType(query)) {
		query.setAbstName(renamer.abstName(query.realName))
		context.setQuery(query)
                true
	      } else false
	    } else false
	  } else if(collectedInfo.isFieldCompletition) {
	    val renamer = context.getRenamer
	    val fieldSymbol = collectedInfo.getFieldSymbol()
	    val field = factory.makeInternalDef(fieldSymbol._1, clazz, fieldSymbol._2)
	    if (typeFinder.setAbstType(field)){
	      field.setAbstName(renamer.abstName(field.realName))
	      context.setField(field)
	      val query = this.factory.makeQuery(collectedInfo.getDesiredType, field)
	      if (typeFinder.setQueryAndLocalAbstType(query)) {
		query.setAbstName(renamer.abstName(query.realName))
		context.setQuery(query)
		true
	      } else false
	    } else false
	  } else false
	} else false
      }

      private def loadLocals(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[LocalInfo]()
	val locals = collectedInfo.getLocalSymbols()

	if (collectedInfo.isLocalCompletition || collectedInfo.isMethodCompletition) {
	  val method = context.getMethod
	  for {
	    local <- locals
	    definition = this.factory.makeLocal(local, method)
	    if (typeFinder.setQueryAndLocalAbstType(definition))
	  } {
	    definition.setAbstName(renamer.abstName(definition.realName))
	    decls = definition :: decls
	  }
	}

        context.setLocals(decls)
      }

      private def loadClass(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[ClauseDefinition]()
	val (declSymbols, classSymbol) = collectedInfo.getClassSymbols()
	val clazz = factory.makeClass(classSymbol)

	for {
	  declSymbol <- declSymbols
	  if (!isInitSymbol(collectedInfo, declSymbol._1))
	  definition = factory.makeInternalDef(declSymbol._1, clazz, declSymbol._2) 
	  if (typeFinder.setAbstType(definition))
	} {
	  definition.setAbstName(renamer.abstName(definition.realName))
	  decls = definition :: decls
	}

        context.setClassDefs(decls ++ addInitDef(collectedInfo, context))
      }

      private def isInitSymbol(collectedInfo:CollectedInfo, decl:Symbol) = {
	if (collectedInfo.isLocalCompletition) decl.fullName.equals(collectedInfo.getMethod.fullName) else false
      }

      private def addInitDef(collectedInfo:CollectedInfo, context:ISynthContext) = {
	if (collectedInfo.isLocalCompletition && !returnsUnit(context.getMethod.realType)) List[ClauseDefinition](context.getMethod) else List[ClauseDefinition]()
      }

      private def loadPackage(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[ClauseDefinition]()
	val declSymbolss = collectedInfo.getPackageSymbols()

	for {
	  (declSymbols, classSymbol) <- declSymbolss
	  clazz = factory.makeClass(classSymbol)
	  declSymbol <- declSymbols
	  definition = factory.makeExternalDef(declSymbol, clazz) 
	  if (typeFinder.setAbstType(definition))
	}{
	  definition.setAbstName(renamer.abstName(definition.realName))
	  decls = definition :: decls
	}

        context.setPackageDefs(decls)
      }

      private def loadImports(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[ClauseDefinition]()
	val declSymbolss = collectedInfo.getImportedSymbols()

	for {
	  (declSymbols, classSymbol) <- declSymbolss
	  clazz = factory.makeClass(classSymbol)
	  declSymbol <- declSymbols
	  definition = factory.makeExternalDef(declSymbol, clazz) 
	  if (typeFinder.setAbstType(definition))
	} {
	  definition.setAbstName(renamer.abstName(definition.realName))
	  decls = definition :: decls
	}

        context.setImportedDefs(decls)
      }


      private def loadNestedClasses(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[ClauseDefinition]()
	val declSymbolss = collectedInfo.getOwnerClassesSymbols()

	for {
	  (declSymbols, classSymbol) <- declSymbolss
	  clazz = factory.makeClass(classSymbol)
	  declSymbol <- declSymbols
	  definition = factory.makeExternalDef(declSymbol, clazz) 
	  if (typeFinder.setAbstType(definition))
	} {
	  definition.setAbstName(renamer.abstName(definition.realName))
	  decls = definition :: decls
	}

	context.setNestedDefs(decls)
      }

      private def loadSuperclasses(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[ClauseDefinition]()
	val declSymbolss = collectedInfo.getSupertypeSymbols()

	for {
	  (declSymbols, classSymbol) <- declSymbolss
	  clazz = factory.makeClass(classSymbol)
	  declSymbol <- declSymbols
	  definition = factory.makeExternalDef(declSymbol, clazz) 
	  if (typeFinder.setAbstType(definition))
	} {
	  definition.setAbstName(renamer.abstName(definition.realName))
	  decls = definition :: decls
	}

        context.setSubtypeDefs(decls)
      }



      private def loadInheritanceDefs(context:ISynthContext, collectedInfo:CollectedInfo, typeFinder:TypeFinder){
	val renamer = context.getRenamer
	var decls = List[ClauseDefinition]()
	val declSymbolss = collectedInfo.getAllLoadedSymbols()

	for {
	  clazz <- declSymbolss
	  subtype = clazz.tpe
	  supertype <- subtype.parents.map(x => subtype.baseType(x.typeSymbol))
	  definition = factory.makeInheritance(subtype, supertype) 
	  if (typeFinder.setInheritanceFunction(definition))
	} {
	  definition.setAbstName(renamer.abstName(definition.realName))
	  decls = definition :: decls
	}

        context.setInheritanceDefs(decls)
      }


      private def loadUserDefs(context:ISynthContext) = {
	val typeSystem = context.getTypeSystem
	val typeFinder = new TypeFinder(typeSystem)
	val renamer = context.getRenamer
	
	val intdef = factory.makeIntUD
	typeFinder.setUserDefAbstType(intdef)
	intdef.setAbstName(renamer.abstName(intdef.realName))

	val booleandef = factory.makeBooleanUD
	typeFinder.setUserDefAbstType(booleandef)
	booleandef.setAbstName(renamer.abstName(booleandef.realName))

	val stringdef = factory.makeStringUD
	typeFinder.setUserDefAbstType(stringdef)
	stringdef.setAbstName(renamer.abstName(stringdef.realName))

	context.setUserDefs(List[ClauseDefinition](intdef, booleandef, stringdef))
      }

      private val unitTupe = Unit.getClass.getName.replace(".runtime.",".").replace("$","")

      private def existsPublicGetter(decl:Symbol, decls:List[Symbol]) = decls.exists(x => x.isGetter && x.isPublic && x.fullName.equals(decl.fullName))

      private def returnsUnit(decl:Symbol) = if (decl.tpe != null && decl.tpe.resultType != null && decl.tpe.resultType.typeSymbol != null) decl.tpe.resultType.typeSymbol.fullName.equals(unitTupe)
					     else false

      private def returnsUnit(tpe:Type) = if (tpe != null && tpe.resultType != null && tpe.resultType.typeSymbol != null) tpe.resultType.typeSymbol.fullName.equals(unitTupe)
					    else false

  }
}
