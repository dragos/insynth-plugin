package ch.epfl.insynth.library.renamers

trait Renamers {

  trait NameGenerator[T >: Null] extends Cloneable {
    def generate():T
    override def clone():NameGenerator[T] = super.clone().asInstanceOf[NameGenerator[T]]
  }

  class SimpleNameGenerator(
    private var base:String
  ) extends NameGenerator[String] {
    private var index:Long = -1

    def this() = this(null)

    def generate() = {
      increment()
      base +"_"+index.toString //"_" is important, because the generator might generate a base for another generator
    }

    def reset() {this.index = -1}

    def decrement(){this.index -= 1}

    def setBase(base:String) {this.base = base}

    def setBaseAndReset(base:String) {
      this.setBase(base)
      this.reset()
    }

    def increment(){this.index +=1}
  }

  /*
   *
   * Class that maps real to abstract names and vice versa.
   *
   */
  trait Renamer[K >: Null, V >: Null] extends Cloneable {

    /*
     *
     * Maps real to abstract names.
     *
     */
    protected var map = Map.empty[K, V]

    /*
     *
     * Maps abstract names to realNames.
     *
     */
    protected var reverseMap = Map.empty[V, K]

    /*
     *
     * Should find an abstract name for a given real name. If not it should generate one with newName method.
     *
     */
    def abstName(name:K):V 

    /*
     *
     * Should find a real name for a given abstract name. If not it should return "null".
     *
     */
    def realName(name:V):K

    def getMap = map

    def getReverseMap = reverseMap

    override def clone() = super.clone().asInstanceOf[Renamer[K, V]]
  }

  class SimpleRenamer[K >: Null, V >: Null](
    private var nameGen:NameGenerator[V]
  ) extends Renamer[K, V] {

    def this() = this(null)

    def abstName(name:K):V = {
      if (map.contains(name)) map(name)
      else newName(name)
    }
 
    def realName(name:V):K = {
      if (reverseMap.contains(name)) reverseMap(name)
      else throw new Exception("No such real name found.")
    }

    private def newName(name:K):V = {
      val newName = nameGen.generate()
      map += name -> newName
      reverseMap += newName -> name
      newName
    }

    def setNameGenerator(nameGen:NameGenerator[V]){this.nameGen = nameGen}

    override def clone():SimpleRenamer[K, V] = {
      val renamer = super.clone().asInstanceOf[SimpleRenamer[K,V]]
      if (this.nameGen != null) renamer.nameGen = this.nameGen.clone()

      //map and reverseMap are immutable, so we do not need to clone them

      renamer
    }
  }

  class CompositeRenamer[K >: Null, V >: Null] extends Renamer[K, V] {

    private var renamers = List[Renamer[K, V]]()

    def abstName(name:K):V = this.renamers.head.abstName(name)
    def realName(name:V):K = this.renamers.head.realName(name)

    override def getMap = this.renamers.head.getMap

    override def getReverseMap = this.renamers.head.getReverseMap

    def addRenamer(renamer:Renamer[K, V]){this.renamers = renamer :: this.renamers}

    def removeRenamer(){this.renamers = this.renamers.tail}

    def topRenamer = this.renamers.head

    override def clone():CompositeRenamer[K, V] = {
      val renamer = super.clone().asInstanceOf[CompositeRenamer[K,V]]
      renamer.renamers = this.renamers.map(x => x.clone())
      renamer
    }
  }
}










