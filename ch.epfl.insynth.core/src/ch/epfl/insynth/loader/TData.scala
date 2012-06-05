package ch.epfl.insynth.loader

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.InSynth

trait TData {
  self:InSynth =>

  import compiler._

class RawData {

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
   * classes on the path to the query point
   */
  private var ownerTypes = List[Symbol]()
  

  /*
   * field whose initializer must be completed
   */
  private var fieldToComplete:Symbol = null

  /*
   * chain of all methods that contain program point
   */
  private var methodToComplete:Symbol = null

  /*
   * local that contain program point
   */
  private var localToComplete:Symbol = null

  /*
   * vals and vars from the last method body prior to var/val that should be completed
   */
  private var localContext = List[Symbol]() 

  private var local:Symbol = null
  
  /*
   * a type of expression that needs to be completed at a given point
   */    
  private var desiredType:Type = null

  def isFieldCompletition = this.fieldCompletition
  def setFieldCompletition(fieldCompletition:Boolean){
    this.fieldCompletition = fieldCompletition
  }
  
  def isMethodCompletition = this.methodCompletition
  def setMethodCompletition(methodCompletition:Boolean){
    this.methodCompletition = methodCompletition
  }

  def isLocalCompletition = this.localCompletition
  def setLocalCompletition(localCompletition:Boolean){
    this.localCompletition = localCompletition
  }

  def hasPackage = this.pkg != null
  def getPackage = this.pkg
  def setPackage(pkg:Symbol){
    this.pkg = pkg
  }

  def hasOwnerTypes = !this.ownerTypes.isEmpty
  def getOwnerTypes = this.ownerTypes
  def addOwnerTypes(tpe:Symbol){
    this.ownerTypes = tpe :: this.ownerTypes
  }
  
  def getMostNestedOwnerType = if (hasOwnerTypes) this.ownerTypes.head else null

  def hasFieldToComplete = this.fieldToComplete != null
  def getFieldToComplete = this.fieldToComplete
  def setFieldToComplete(field:Symbol){
    this.fieldToComplete = field
    this.fieldCompletition = true
  }

  def hasMethodToComplete = this.methodToComplete != null
  def getMethodToComplete = this.methodToComplete
  def setMethodToComplete(method:Symbol){
    this.methodToComplete = method
    this.methodCompletition = true
  }

  def hasLocalToComplete = this.localToComplete != null
  def getLocalToComplete = this.localToComplete
  def setLocalToComplete(local:Symbol){
    this.localToComplete = local
    this.localCompletition = true
  }    

  def hasImports = !this.imports.isEmpty
  def getImports = this.imports
  def addImport(imp:Import){
    this.imports = imp :: this.imports
  }

  def hasLocalContext = !this.localContext.isEmpty
  def getLocalContext = this.localContext
  def addToLocalContext(decl:Symbol){
    this.localContext = decl :: this.localContext.filterNot(x => decl.fullName.equals(x.fullName)) //TODO: or simpleName?
  }

  def hasDesiredType = this.desiredType != null
  def getDesiredType = this.desiredType
  def setDesiredType(desiredType:Type){
    this.desiredType = desiredType
  }
}
  
  
  class SimpleDecl(symbol:Symbol, receiver:Symbol, inObject:Boolean, hasThis:Boolean, constructor:Boolean) {
    def getSymbol = symbol
    def getReceiver = receiver
    def needThis = hasThis
    def isInObject = inObject
    def isConstructor = constructor
    def needReceiver = !inObject && !constructor
    def needParentheses:Boolean = this.symbol.tpe.paramSectionCount != 0
    def isApply = this.symbol.simpleName.toString.equals("apply")
  }
    
  class Coerction(subclass:Symbol, superclass:Symbol) {
    def getSubclass = subclass
    def getSubtype = subclass.tpe
    def getSuperclass = superclass
    def getSupertype = superclass.tpe
  }
  
  class Data {
    
    //Load all of them
    private var typeDecls = List.empty[SimpleDecl]
    
    //Load only public decls of these classes
    private var types = List.empty[Symbol]
    
    //Load only public decls of these classes
    private var coerctions = List.empty[Coerction]
    
    //Load all of them
    private var locals = List.empty[Symbol]
    
    //type of "this"
    private var thisType:Type = null
    
    //a user desired type
    private var desiredType:Type = null    
    
    def getMostNestedOwnerTypeDecls = typeDecls
    def setMostNestedOwnerTypeDecls(typeDecls:List[SimpleDecl]) {
      this.typeDecls = typeDecls
    }
    
    //All except the most nested owner type
    def getTypes = types
    def setTypes(types:List[Symbol]) {
      this.types = types
    }
    
    def getCoerctions = coerctions
    def setCoerctions(coerctions:List[Coerction]) {
      this.coerctions = coerctions
    }
    
    def getLocals = locals
    def setLocals(locals:List[Symbol]){
      this.locals = locals
    }
    
    def getThisType = thisType
    def hasThisType = thisType != null
    def setThisType(thisType:Type){
      this.thisType = thisType
    }
    
    def getDesiredType = desiredType
    def hasDesiredType = desiredType != null
    def setDesiredType(desiredType:Type) {
      this.desiredType = desiredType
    }
  }
  
  object Transform {
    
    def apply(rdata:RawData):Data = {
      val data = new Data()

      if (rdata.hasDesiredType) {

        //0) find desired type
        data.setDesiredType(rdata.getDesiredType)
      
        //1) find all visible classes (without the inner most class) + all super-classes
        //2) find inherited functions
        //3) find all local decls
        val locals = filterLocalDecls(rdata)
        data.setLocals(locals)

        //4) find decls for the most-inner class, some of them may be invisible due to the local decls, or invisible field that needs to be completed and in local completition local val      
        val tpeDecls = typeSimpleDecls(rdata, locals)
        data.setMostNestedOwnerTypeDecls(tpeDecls)
      
        //5) find "this"
        
        if (rdata.hasOwnerTypes) {
          val mostNestedOwnerType = rdata.getMostNestedOwnerType
      
          if (!mostNestedOwnerType.isModule){      
            data.setThisType(mostNestedOwnerType.tpe)
          }
        }
      }
      
      data
    }
    
    def filterLocalDecls(rdata:RawData) = {
      val locals = rdata.getLocalContext
      val localToComplete = rdata.getLocalToComplete
      
      if (rdata.isLocalCompletition) locals.filterNot(x => localToComplete.fullName.equals(x.fullName))
      else locals
    }
    
    def typeSimpleDecls(rdata:RawData, locals:List[Symbol]):List[SimpleDecl] = {
      if (rdata.hasOwnerTypes) {
        val tpe = rdata.getMostNestedOwnerType
        var symbols = List[Symbol]()
      
        val decls = tpe.tpe.decls.toList
        
        for {
	      decl <- decls
	      
          if(!decl.nameString.contains("$") && 
	          decl.exists && 
	         !decl.isSynthetic &&
	         !(tpe.isModule && decl.isConstructor) &&
	         !decl.isGetter &&
	         !decl.isSetter &&
	          decl.isValue &&  //What was this? I guess with this we get rid of type defs and other junk.
	         !returnsUnit(decl))
        } yield new SimpleDecl(decl, tpe, tpe.isModule, filterTypeDecl(decl, locals, rdata), decl.isConstructor)
        
      } else Nil
    }
    
    private def filterTypeDecl(decl:Symbol, locals:List[Symbol], rdata:RawData) = {
      !decl.isConstructor && 
      (locals.exists(y => simpleName(y).equals(simpleName(decl))) || 
       (rdata.isLocalCompletition && rdata.getLocalToComplete.fullName.equals(decl.fullName)) ||
       (rdata.isMethodCompletition && rdata.getMethodToComplete.fullName.equals(decl.fullName)) ||
       (rdata.isFieldCompletition && rdata.getFieldToComplete.fullName.equals(decl.fullName)))
    }
    
    private def simpleName(decl:Symbol) = decl.simpleName.toString.replace(" ", "")
    
    private val unitTupe = Unit.getClass.getName.replace(".runtime.",".").replace("$","")

    private def returnsUnit(decl:Symbol) = 
      if (decl.tpe != null &&
          decl.tpe.resultType != null &&
          decl.tpe.resultType.typeSymbol != null) decl.tpe.resultType.typeSymbol.fullName.equals(unitTupe)
	    else false

  }
}