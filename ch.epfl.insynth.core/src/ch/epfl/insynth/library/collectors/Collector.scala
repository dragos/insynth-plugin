package ch.epfl.insynth.library.collectors

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.library.ISynth


trait Collectors {
  self: ISynth =>
    
  import compiler._
  /**********************************************************************************************
  *
  *  Here we store the info from AST tree on the way toward the comletion point
  *
  ******************************************************************************************** */ 

  class CollectedInfo {

    private var fieldCompletition = false 
    private var methodCompletition = false
    private var localCompletition = false

    /*
     * imports
     */
    private var imports = List[Import]()

    /*
     * package 
     */
    private var pkg:Symbol = null

    /*
     * chain of nested classes
     */
    private var ownerClasses = List[Symbol]()

    /*
     * top-level class 
     */
    private var topClass:Symbol = null

    /*
     * field whose initializer must be completed
     */
    private var field:Symbol = null

    /*
     * chain of all methods that contain program point
     */
    private var method:Symbol = null

    /*
     * local that contain program point
     */
    private var local:Symbol = null

    /*
     * vals and vars from the last method body prior to var/val that should be completed
     */
    private var partialMethodBody = List[Symbol]() 

    /*
     * a type of expression that needs to be completed at a given point
     */    
    private var desiredType:Type = null

    def isFieldCompletition = this.fieldCompletition
    def setFieldCompletition(fieldCompletition:Boolean){this.fieldCompletition = fieldCompletition}

    def isMethodCompletition = this.methodCompletition
    def setMethodCompletition(methodCompletition:Boolean){this.methodCompletition = methodCompletition}

    def isLocalCompletition = this.localCompletition
    def setLocalCompletition(localCompletition:Boolean){this.localCompletition = localCompletition}

    def hasPackage = this.pkg != null
    def getPackage:Symbol = this.pkg
    def setPackage(pkg:Symbol){this.pkg = pkg}

    def hasTopClass = this.topClass != null
    def getTopClass:Symbol = this.topClass
    def setTopClass(topClass:Symbol){this.topClass = topClass}

    def hasOwnerClasses = !this.ownerClasses.isEmpty
    def getOwnerClasses:List[Symbol] = this.ownerClasses
    def addOwnerClass(clazz:Symbol){this.ownerClasses = clazz :: this.ownerClasses}

    def hasField = this.field != null
    def getField = this.field
    def setField(field:Symbol){this.field = field}

    def hasMethod = this.method != null
    def getMethod = this.method
    def setMethod(method:Symbol){this.method = method}

    def hasLocal = this.local != null
    def getLocal = this.local
    def setLocal(local:Symbol){this.local = local}    

    def hasImport = !this.imports.isEmpty
    def getImports = this.imports
    def addImport(imp:Import){
      this.imports = imp :: this.imports
    }

    def hasPartialMethodBody = !this.partialMethodBody.isEmpty
    def getPartialMethodBody = this.partialMethodBody
    def addToPartialMethodBody(definition:Symbol){
      this.partialMethodBody.find(x => definition.fullName.equals(x.fullName)) match {
	case Some(oldDef) => {
	  this.partialMethodBody = this.partialMethodBody.filterNot(_ == oldDef)

	}
	case None =>
      }
      this.partialMethodBody = definition :: this.partialMethodBody
    }

    def hasDesiredType = this.desiredType != null
    def getDesiredType = this.desiredType
    def setDesiredType(desiredType:Type){this.desiredType = desiredType}

    override def toString = {
      var s = if (this.isMethodCompletition) "MethodCompletition\n"
	      else if(this.isFieldCompletition) "FieldCompletition\n"
	      else if(this.isLocalCompletition) "LocalCompletition\n"
	      else "No Completition\n"
      
      if (hasTopClass){
	s += "TopClass "+topClass.fullName+"\n"
	if(hasField) s += "Field "+field.fullName+"\n"
	if(hasMethod) s += "Method "+method.fullName+"\n"
	if(hasLocal) s += "Local "+local.fullName+"\n"
	if (hasPartialMethodBody){
	  s+="PMB: "
	  partialMethodBody foreach {
	    x => s += (x.fullName +" ")
	  }
	  s += "\n"
	}
      }
      s
    }

    def getLocalSymbols():List[Symbol] = {
      if (localCompletition) this.methodAgrsAndLocals()
      else if (methodCompletition) this.method.tpe.params 
      else Nil
    }

    private def methodAgrsAndLocals():List[Symbol] = {
      var locals = List[Symbol]()
      val bodyLocals = methodBodyLocals()
      locals ++= bodyLocals
      this.method.tpe.params.foreach{
	x => 
	  if (!bodyLocals.exists(y => y.simpleName.toString.equals(x.simpleName.toString)) && !x.simpleName.toString.equals(local.simpleName.toString))
	  locals = x :: locals
      }
      locals
    }

    private def methodBodyLocals():List[Symbol] = {
      var locals = List[Symbol]()
      this.partialMethodBody.foreach{
	x =>
	  if (!x.simpleName.toString.equals(local.simpleName.toString))
	    locals = x :: locals
      }
      locals
    }

    def getClassSymbols():(List[(Symbol, Boolean)], Symbol) = {
      var symbols = List[(Symbol, Boolean)]()
      if (this.localCompletition) {
	//use local and partialMethodBody, method params
	val localForbidden = visibleLocals()
	val decls = this.topClass.tpe.decls.toList

	for {
	  decl <- decls
	  if (!decl.nameString.contains("$") && 
	      decl.exists && 
	      !decl.isSynthetic &&
	      !(this.topClass.isModule && decl.isConstructor) &&
	      !decl.isGetter &&
	      !decl.isSetter &&
	      decl.isValue &&  //What was this? I guess with this we get rid of type defs and other junk.
	      !returnsUnit(decl))
	} {	  
	  symbols = (decl, localForbidden.exists(decl.simpleName.toString.replace(" ","").equals)) :: symbols
	}
      } else if (this.methodCompletition) {
	//use local and partialMethodBody, method params
	val localForbidden = visibleLocals()
	val decls = this.topClass.tpe.decls.toList

	for {
	  decl <- decls
	  if (!decl.fullName.equals(this.method.fullName) && //we do not allow methodToComplete to appear in decls
	      !decl.nameString.contains("$") &&
	      decl.exists &&
	      !decl.isSynthetic &&
	      !(this.topClass.isModule && decl.isConstructor) &&
	      !decl.isGetter &&
	      !decl.isSetter &&
	      decl.isValue &&
	      !returnsUnit(decl))
	} symbols = (decl, localForbidden.exists(decl.simpleName.toString.replace(" ","").equals)) :: symbols
      } else if (this.fieldCompletition) {
	val decls = this.topClass.tpe.decls.toList

	for {
	  decl <- decls
	  if (!decl.fullName.equals(this.field.fullName) &&  //we do not allow fieldToComplete to appear in decls
	      !decl.nameString.contains("$") &&
	      decl.exists &&
	      !decl.isSynthetic &&
	      !(this.topClass.isModule && decl.isConstructor) &&
	      !decl.isGetter &&
	      !decl.isSetter &&
	      decl.isValue &&
	      !returnsUnit(decl))
	} symbols = (decl, false) :: symbols
      }
      (symbols, topClass)
    }

    def getTopClassSymbol = this.getTopClass

    def getMethodSymbol():(Symbol, Boolean) = {
      if (this.hasMethod){
       val localForbidden = visibleLocals()
       (this.method, localForbidden.exists(this.method.simpleName.toString.replace(" ","").equals))
      } else (null, false)    
    }

    def getFieldSymbol():(Symbol, Boolean) = {
      if (this.hasField){
       val localForbidden = visibleLocals()
       (this.field, localForbidden.exists(this.field.simpleName.toString.replace(" ","").equals))
      } else (null, false)
    }

    private var packageSymbols = List[Symbol]() 
    private var importedSymbols = List[Symbol]() 
    private var ownerSymbols = List[Symbol]() 
    private var subtypeSymbols = List[Symbol]()

    def getAllLoadedSymbols() = topClass :: (packageSymbols ++ importedSymbols ++ ownerSymbols ++ subtypeSymbols)

    def getSupertypeSymbols():List[(List[Symbol], Symbol)] = {
      var symbols = List[(List[Symbol], Symbol)]()
      var classes = List[Symbol]()
      var topLevelSymbols = topClass :: (packageSymbols ++ importedSymbols ++ ownerSymbols)
      var set = topLevelSymbols.map(x=>x.fullName)
      var workingList = topLevelSymbols

      while(!workingList.isEmpty){
	var curr = workingList.head
	workingList = workingList.tail
	var superClasses = curr.tpe.parents.map(x => x.typeSymbol).toSet
	superClasses = superClasses.filterNot(x => set.contains(x.fullName))
	workingList ++= superClasses
	set ++= superClasses.map(x => x.fullName)
	subtypeSymbols ++= superClasses
      }

      subtypeSymbols.foreach{
	clazz =>
	  symbols = (loadClass(clazz), clazz) :: symbols
      }

      symbols
    }

    def getPackageSymbols():List[(List[Symbol], Symbol)] = {
      var symbols = List[(List[Symbol], Symbol)]()
      for {
	clazz <- this.pkg.tpe.decls
	if (!clazz.nameString.contains("$")
	    && clazz.exists
	    && !clazz.fullName.equals(topClass.fullName)
	    && (clazz.isClass || clazz.isModule || clazz.isAbstractClass || clazz.isTrait)
	    && !clazz.isSynthetic
	    && !clazz.isAbstractType
	    && !clazz.isPackage)
      } {
	val loadedDecls = loadClass(clazz)

	symbols = (loadedDecls, clazz) :: symbols
	packageSymbols = clazz :: packageSymbols
       }
      symbols

    }

    def getImportedSymbols():List[(List[Symbol], Symbol)] = {
      var symbols = List[(List[Symbol], Symbol)]()

      for{imp <- this.imports
	  val clazz <- imp.expr.tpe.decls//.flatMap(transformImport(imp.selectors, _))
  	  if (!clazz.nameString.contains("$") 
	    && clazz.exists
	    && !clazz.fullName.equals(this.topClass.fullName)
	    && (clazz.isClass || clazz.isModule || clazz.isAbstractClass || clazz.isTrait)
	    && !clazz.isSynthetic
	    && !clazz.isAbstractType 
	    && !clazz.isPackage
	    )
      } {
	symbols = (loadClass(clazz), clazz) :: symbols
	importedSymbols = clazz :: importedSymbols
      }

      symbols
    }


    // TODO: forbid those whose name is covered by some deeper name symbol
    def getOwnerClassesSymbols():List[(List[Symbol], Symbol)] = {
      var symbols = List[(List[Symbol], Symbol)]()
      if (this.hasOwnerClasses){
      
	//find all classes
	var classes = findAllDeclaredClassesInOwnerClasses()
	for {
	  clazz <- classes
	  if (!clazz.nameString.contains("$") 
	    && clazz.exists
	    && !clazz.fullName.equals(this.topClass.fullName)
	    && (clazz.isClass || clazz.isModule || clazz.isAbstractClass || clazz.isTrait)
	    && !clazz.isSynthetic
	    && !clazz.isAbstractType 
	    && !clazz.isPackage)
	} {
	  symbols = (loadClass(clazz), clazz) :: symbols
	  ownerSymbols = clazz :: ownerSymbols
	}
      }
      symbols
    }

    private def findAllDeclaredClassesInOwnerClasses():List[Symbol] = {
      var symbols = List[Symbol]()

      this.ownerClasses.foreach{
	x => 
	  if(x.isClass || x.isModule) symbols = x :: symbols

	  (x.tpe.members.toList++x.thisSym.tpe.decls.toList).foreach{
	    y => 
	      if (this.ownerClasses.forall{z => !y.fullName.equals(z.fullName)}
	          && (y.isClass || y.isModule)) 
		symbols = y :: symbols
	  }
      }
      symbols
    }

    private def visibleLocals(): Set[String] = {
      var forbidden = Set[String]()

      if(this.localCompletition){
	forbidden += this.local.simpleName.toString
	forbidden ++= this.methodAgrsAndLocals().map(x => x.simpleName.toString)
      } else if (this.methodCompletition) {
	forbidden ++= this.method.tpe.params.map(x => x.simpleName.toString)
      }

      forbidden
    }

    private def loadClass(clazz:Symbol):List[Symbol] = {
      var symbols = List[Symbol]()
      //val superClass = clazz.superClass 
      //TODO:Think about adding members.
      //See why some constructors appear.
      //val decls = clazz.tpe.decls.toList ++ clazz.tpe.members.toList.filter(x => x.fullName.contains(superClass.fullName))

      val decls = clazz.tpe.decls.toList
      for {
	decl <- decls
	if (!decl.nameString.contains("$") && decl.exists && !decl.isSynthetic && !((clazz.isModule || clazz.isTrait || clazz.isAbstractClass) && decl.isConstructor) && !decl.isOverride && !decl.isIncompleteIn(clazz) && decl.isValue && !decl.isGetter && !decl.isSetter && !returnsUnit(decl))
      }{
	if (decl.isMethod && decl.isPublic){
	  if (!decl.fullName.contains("asInstanceOf"))
	    symbols = decl :: symbols
	} else if (decl.isPublic || existsPublicGetter(decl, decls)){
	  symbols = decl :: symbols
	}
      }


      symbols
    }

    private val unitTupe = Unit.getClass.getName.replace(".runtime.",".").replace("$","")

    private def returnsUnit(decl:Symbol) = if (decl.tpe != null && decl.tpe.resultType != null && decl.tpe.resultType.typeSymbol != null) decl.tpe.resultType.typeSymbol.fullName.equals(unitTupe)
					   else false

    private def existsPublicGetter(decl:Symbol, decls:List[Symbol]) = decls.exists(x => x.isGetter && x.isPublic && x.fullName.equals(decl.fullName))

    private def existsPublicField(decl:Symbol, decls:List[Symbol]) = decls.exists(x => !x.isMethod && x.isPublic && x.fullName.equals(decl.fullName))

    private def transformImport(selectors: List[ImportSelector], sym: Symbol): List[Symbol] = selectors match {
      case List() => List()
      case List(ImportSelector(nme.WILDCARD, _, _, _)) => List(sym)
      case ImportSelector(from, _, to, _) :: _ if (from.toString == sym.name.toString) =>
	if (to == nme.WILDCARD) List()
	else { val sym1 = sym.cloneSymbol; sym1.name = to; List(sym1) }
      case _ :: rest => transformImport(rest, sym)
    }
  }

  /**********************************************************************************************
  *
  *                Extracts path from a top-class to a given position
  *
  ******************************************************************************************** */ 

  class Collector {
    protected var currentOwner: Symbol = null
    private var pos:Position = null
    private var info:CollectedInfo = null
    private var lastTraversedTree:Tree = null 

    def collect(pos:Position, tree:Tree): CollectedInfo = {
      this.pos = pos
      println("Position: "+pos)
      
      this.info = new CollectedInfo()
      this.traverse(tree)
      this.info
    }

    private def traverse(tree:Tree) {
/*    
      println("---------------------------------------------------------------------------------")  
      println("Type: "+tree.getClass.getName)
      println("Pos: "+tree.pos)
      println(tree)
      println("---------------------------------------------------------------------------------") 
*/    
      if (tree.pos.includes(pos)) {
	lastTraversedTree = tree
	tree match {
	  case EmptyTree =>
            ;
	  case PackageDef(pid, stats) =>
	    this.extractImports(stats)
            this.traverse(pid)
            atOwner(tree.symbol.moduleClass) {
	      this.info.setPackage(currentOwner)
              this.traverseTrees(stats)
            }
	  case ClassDef(mods, name, tparams, impl) =>
            atOwner(tree.symbol) {
	      if (this.info.hasTopClass){
		this.info.addOwnerClass(this.info.getTopClass)
	      }
	      this.info.setTopClass(currentOwner)
              this.traverseTrees(mods.annotations)
	      this.traverseTrees(tparams)
	      this.traverse(impl)
            }
	  case ModuleDef(mods, name, impl) =>
            atOwner(tree.symbol.moduleClass) {
	      if (this.info.hasTopClass){
		this.info.addOwnerClass(this.info.getTopClass)
	      }
              this.info.setTopClass(currentOwner)
	      this.traverseTrees(mods.annotations)
	      this.traverse(impl)
            }
	  case ValDef(mods, name, tpt:TypeTree, rhs) =>
            atOwner(tree.symbol) {
              println("1:**"+rhs+"**")
       
            }
	  case ValDef(mods, name, tpt , rhs) =>
            atOwner(tree.symbol) {
              println("2:**"+rhs+"**")
              
            }
	  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            atOwner(tree.symbol) {
              this.traverseTrees(mods.annotations)
	      this.traverseTrees(tparams)
	      this.traverseTreess(vparamss)
	      this.traverse(tpt)
	      this.info.setMethod(currentOwner)		
	      this.traverseBlock(rhs)

            }
	  case TypeDef(mods, name, tparams, rhs) =>
            atOwner(tree.symbol) {
              this.traverseTrees(mods.annotations)
	      this.traverseTrees(tparams)
	      this.traverse(rhs)
            }
	  case LabelDef(name, params, rhs) =>
            this.traverseTrees(params)
	    this.traverse(rhs)
	  case imp @ Import(expr, selectors) =>
	    this.info.addImport(imp)
            this.traverse(expr)
	  case Annotated(annot, arg) =>
            this.traverse(annot)
	    this.traverse(arg)
	  case Template(parents, self, body) =>
            this.traverseTrees(parents)
            if (!self.isEmpty) this.traverse(self)
            this.traverseStats(body, tree.symbol)
	  case Block(stats, expr) =>
            this.traverseTrees(stats)
	    this.traverse(expr)
	  case CaseDef(pat, guard, body) =>
            this.traverse(pat)
	    this.traverse(guard)
	    this.traverse(body)
	  case Alternative(trees) =>
            this.traverseTrees(trees)
	  case Star(elem) =>
            this.traverse(elem)
	  case Bind(name, body) =>
            this.traverse(body)
	  case UnApply(fun, args) =>
            this.traverse(fun)
	    this.traverseTrees(args)
	  case ArrayValue(elemtpt, trees) =>
            this.traverse(elemtpt)
	    this.traverseTrees(trees)
	  case Function(vparams, body) =>
            atOwner(tree.symbol) {
              this.traverseTrees(vparams)
	      this.traverse(body)
            }
	  case Assign(lhs, rhs) =>
            this.traverse(lhs)
	    this.traverse(rhs)
	  case If(cond, thenp, elsep) =>
            this.traverse(cond)
	    this.traverse(thenp)
	    this.traverse(elsep)
	  case Match(selector, cases) =>
            this.traverse(selector)
	    this.traverseTrees(cases)
	  case Return(expr) =>
            this.traverse(expr)
	  case Try(block, catches, finalizer) =>
            this.traverse(block)
	    this.traverseTrees(catches)
	    this.traverse(finalizer)
	  case Throw(expr) =>
            this.traverse(expr)
	  case New(tpt) =>
            this.traverse(tpt)
	  case Typed(expr, tpt) =>
            this.traverse(expr)
	    this.traverse(tpt)
	  case TypeApply(fun, args) =>
            this.traverse(fun)
	    this.traverseTrees(args)
	  case Apply(fun, args) =>
            this.traverse(fun)
	    this.traverseTrees(args)
	  case ApplyDynamic(qual, args) =>
            this.traverse(qual)
	    this.traverseTrees(args)
	  case Super(_, _) =>
            ;
	  case This(_) =>
            ;
	  case Select(qualifier, selector) =>
            traverse(qualifier)
	  case Ident(_) =>
            ;
	  case Literal(_) =>
            ;
	  case TypeTree() =>
            ;
	  case SingletonTypeTree(ref) =>
            this.traverse(ref)
	  case SelectFromTypeTree(qualifier, selector) =>
            this.traverse(qualifier)
	  case CompoundTypeTree(templ) =>
            this.traverse(templ)
	  case AppliedTypeTree(tpt, args) =>
            this.traverse(tpt)
	    this.traverseTrees(args)
	  case TypeBoundsTree(lo, hi) =>
            this.traverse(lo)
	    this.traverse(hi)
	  case ExistentialTypeTree(tpt, whereClauses) =>
            this.traverse(tpt)
	    this.traverseTrees(whereClauses)
	  case SelectFromArray(qualifier, selector, erasure) =>
            this.traverse(qualifier)
	}
      }
      else
	//if (tree.pos.startOrPoint > pos.startOrPoint && 
	 //   pos.startOrPoint > lastTraversedTree.pos.endOrPoint)
	  if(this.isCompletition(tree))
	  lastTraversedTree match {
	    case ValDef(mods, name, tpt:TypeTree, rhs) =>
	      atOwner(lastTraversedTree.symbol) {
		this.info.setField(currentOwner)
		this.info.setFieldCompletition(true)
		this.info.setDesiredType(tpt.tpe)
	      }
	    case DefDef(mods, name, tparams, vparamss, tpt:TypeTree, rhs) =>
	      atOwner(lastTraversedTree.symbol) {
		this.info.setMethod(currentOwner)
		this.info.setMethodCompletition(true)
		this.info.setDesiredType(tpt.tpe)
	      }
	    case _ => throw new Exception("No such auto-complete."+ lastTraversedTree) 
	  }
	else
	  lastTraversedTree = tree
    }

    private def traverseBlock(tree:Tree) {
      if (tree.pos.includes(pos)){
      //out.write(tree.toString+"\n")  
      tree match { 
	case EmptyTree =>
          ;
	case PackageDef(pid, stats) =>
          this.traverseBlock(pid)
        atOwner(tree.symbol.moduleClass) {
          this.traverseBlockTrees(stats)
        }
	case ClassDef(mods, name, tparams, impl) =>
          atOwner(tree.symbol) {
	    this.info.setTopClass(currentOwner)
            this.traverseBlockTrees(mods.annotations)
	    this.traverseBlockTrees(tparams)
	    this.traverseBlock(impl)
          }
	case ModuleDef(mods, name, impl) =>
          atOwner(tree.symbol.moduleClass) {
            this.info.setTopClass(currentOwner)
	    this.traverseBlockTrees(mods.annotations)
	    this.traverseBlock(impl)
          }
	case ValDef(mods, name, tpt:TypeTree, rhs) =>
          atOwner(tree.symbol) {
          }
	case ValDef(mods, name, tpt , rhs) =>
          atOwner(tree.symbol) {
          }
	case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          atOwner(tree.symbol) {
            this.traverseBlockTrees(mods.annotations)
	    this.traverseBlockTrees(tparams)
	    this.traverseBlockTreess(vparamss)
	    this.traverseBlock(tpt)
	    if (isBlock(rhs)){
	      this.traverseBlock(rhs)
	    }
          }
	case TypeDef(mods, name, tparams, rhs) =>
          atOwner(tree.symbol) {
            this.traverseBlockTrees(mods.annotations)
	    this.traverseBlockTrees(tparams)
	    this.traverseBlock(rhs)
          }
	case LabelDef(name, params, rhs) =>
          this.traverseBlockTrees(params)
	  this.traverseBlock(rhs)
	case Import(expr, selectors) =>
          this.traverseBlock(expr)
	case Annotated(annot, arg) =>
          this.traverseBlock(annot)
	  this.traverseBlock(arg)
	case Template(parents, self, body) =>
          this.traverseBlockTrees(parents)
          if (!self.isEmpty) this.traverseBlock(self)
          this.traverseBlockStats(body, tree.symbol)
	case Block(stats, expr) =>
	  val list:List[Tree] = stats ::: List[Tree](expr)
	  val length = list.size
	  var i = 0
	  while(i < length && (list(i).pos.startOrPoint < pos.startOrPoint || list(i).pos.includes(pos))){
	    val statement = list(i)
	    if (statement.pos.includes(pos)){
	      traverseBlock(statement)
	    } else {
	      if (i+1 < length && this.isCompletition(list(i+1))){
	    
	    //if(statement.pos.includes(pos))
	    
	    //if(this.isCompletition(statement)){
		statement match {
		  case ValDef(_,_,tpt:TypeTree,_) =>
		    this.info.setLocal(statement.symbol)    
		    this.info.setDesiredType(tpt.tpe)
		    this.info.setLocalCompletition(true)
		  case If(cond, thenp, elsep) if(cond.symbol == null) =>
		    //ugly hack
		    this.info.setLocal(definitions.BooleanClass)
		    this.info.setDesiredType(definitions.BooleanClass.tpe)
		    this.info.setLocalCompletition(true)
		  case Apply(fun, args) =>
		    args.foreach{x => 
		      x match {
			case If(cond, thenp, elsep) if(cond.symbol == null) =>
			  this.info.setLocal(definitions.BooleanClass)
			  this.info.setDesiredType(definitions.BooleanClass.tpe)
			  this.info.setLocalCompletition(true)
			case _ => 
		      }
		    }
		  case DefDef(_,_,_,_,_,_) =>
		    //TODO support methods
		  case _ =>
		}
	      }
	      else
		if (isValDef(statement)){ 
		  this.info.addToPartialMethodBody(statement.symbol)
		}
	    }
	    i+=1
	  }

	case CaseDef(pat, guard, body) =>
	  if (body.pos.includes(pos)){
	  pat match{
	    case Apply(fun, args) =>
	      args.foreach{x => x match {case Bind(name, body) => this.info.addToPartialMethodBody(x.symbol)
					 case _=>}
			 }
	      case _ =>
	    }
	  }

          this.traverseBlock(pat)
	  this.traverseBlock(guard)
	  this.traverseBlock(body)
	case Alternative(trees) =>
          this.traverseBlockTrees(trees)
	case Star(elem) =>
          this.traverseBlock(elem)
	case Bind(name, body) =>
          this.traverseBlock(body)
	case UnApply(fun, args) =>
          this.traverseBlock(fun)
	  this.traverseBlockTrees(args)
	case ArrayValue(elemtpt, trees) =>
          this.traverseBlock(elemtpt)
	  this.traverseBlockTrees(trees)
	case Function(vparams, body) =>
          atOwner(tree.symbol) {
            this.traverseBlockTrees(vparams)
	    this.traverseBlock(body)
          }
	case Assign(lhs, rhs) =>
          this.traverseBlock(lhs)
	  this.traverseBlock(rhs)
	case If(cond, thenp, elsep) =>
          this.traverseBlock(cond)
	  this.traverseBlock(thenp)
	  this.traverseBlock(elsep)
	case Match(selector, cases) =>
          this.traverseBlock(selector)
	  this.traverseBlockTrees(cases)
	case Return(expr) =>
          this.traverseBlock(expr)
	case Try(block, catches, finalizer) =>
          this.traverseBlock(block)
	  this.traverseBlockTrees(catches)
	  this.traverseBlock(finalizer)
	case Throw(expr) =>
          this.traverseBlock(expr)
	case New(tpt) =>
          this.traverseBlock(tpt)
	case Typed(expr, tpt) =>
          this.traverseBlock(expr)
	  this.traverseBlock(tpt)
	case TypeApply(fun, args) =>
          this.traverseBlock(fun)
	  this.traverseBlockTrees(args)
	case Apply(fun, args) =>
          this.traverseBlock(fun)
	  this.traverseBlockTrees(args)
	case ApplyDynamic(qual, args) =>
          this.traverseBlock(qual)
	  this.traverseBlockTrees(args)
	case Super(_, _) =>
          ;
	case This(_) =>
          ;
	case Select(qualifier, selector) =>
          this.traverseBlock(qualifier)
	case Ident(_) =>
          ;
	case Literal(_) =>
          ;
	case TypeTree() =>
          ;
	case SingletonTypeTree(ref) =>
          this.traverseBlock(ref)
	case SelectFromTypeTree(qualifier, selector) =>
          this.traverseBlock(qualifier)
	case CompoundTypeTree(templ) =>
          this.traverseBlock(templ)
	case AppliedTypeTree(tpt, args) =>
          this.traverseBlock(tpt)
	  this.traverseBlockTrees(args)
	case TypeBoundsTree(lo, hi) =>
          this.traverseBlock(lo)
	  this.traverseBlock(hi)
	case ExistentialTypeTree(tpt, whereClauses) =>
          this.traverseBlock(tpt)
	  this.traverseBlockTrees(whereClauses)
	case SelectFromArray(qualifier, selector, erasure) =>
          this.traverseBlock(qualifier)
      }
    }
  }
  
    private def traverseBlockTrees(trees: List[Tree]) {
      trees foreach traverseBlock
    }

    private def traverseBlockTreess(treess: List[List[Tree]]) {
      treess foreach traverseBlockTrees
    }

    private def traverseBlockStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner) atOwner(exprOwner)(traverseBlock(stat))
        else traverseBlock(stat)
      )
    }

    private def traverseTrees(trees: List[Tree]) {
      trees foreach traverse
    }

    private def traverseTreess(treess: List[List[Tree]]) {
      treess foreach traverseTrees
    }

    private def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner) atOwner(exprOwner)(traverse(stat))
        else traverse(stat)
      )
    }

    private def atOwner(owner: Symbol)(traverse: => Unit) {
      val prevOwner = currentOwner
      currentOwner = owner
      traverse
      currentOwner = prevOwner
    }

    private def isCompletition(tree:Tree) = tree match {
      case t @ Apply(_,_) => {
        t.toString.equals("scala.this{type}.Predef.exit{()Nothing}(){Nothing}")//scala.this.Predef.exit()")
      }
      case t => {
        false
      }
    }

    private def isBlock(tree:Tree) = tree match {
      case Block(_,_) => true
      case _ => false
    }

    private def isMethod(tree:Tree) = tree match {
      case DefDef(_,_,_,_,_,_) => true
      case _ => false
    }

    private def isValDef(tree:Tree) = tree match {
      case ValDef(_,_,_,_) => true
      case _ => false
    }

    private def isMatch(tree:Tree) = tree match {
      case Match(_, _) => true
      case _ => false
    }

    private def isTry(tree:Tree) = tree match {
      case Try(_, _, _) => true
      case _ => false
    }
    
    private def extractImports(trees:List[Tree]) {
      trees.foreach{
	_ match {
	  case imp @ Import(_,_) => this.info.addImport(imp)
	  case _ => 
	}
      }
    }
  }
}
