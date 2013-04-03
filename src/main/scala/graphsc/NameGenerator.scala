package graphsc

// Generates somewhat readable names
class NameGenerator[T](length: Int = 5) {
  val val2name = collection.mutable.Map[T, String]()
  //val name2val = collection.mutable.Map[String, T]()
  
  def apply(v: T): String =
    val2name.getOrElseUpdate(v, {
      var seed = v.hashCode
      var res: String = null
      while(res == null) {
        var word = ""
        var state: Boolean = true
        val rnd = new util.Random(seed)  
        
        for(_ <- 0 until length) {
          if(state)
            word += "wrtypsdfghjklzxcvbnm"(rnd.nextInt(20))
          else
            word += "euioa"(rnd.nextInt(5))
            
          state = !state
        }
          
        if(!val2name.exists(_._2 == word))
          res = word
        
        seed += 1
      }
      res
    })
}