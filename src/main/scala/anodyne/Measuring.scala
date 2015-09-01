package anondyne

object Measuring {

  def timer(t0: Long = 0l) = System.currentTimeMillis - t0
   
  def benchmark[U](warmups: Int, trials: Int)(f: => U) = {
    
    var i = 0
    while (i < warmups) {
      f
      i += 1
    }

    val t0 = System.nanoTime()
    i = 0
    while (i < trials) {
      f
      i += 1
    }
    val t1 = System.nanoTime()

    (t1 - t0) / 1000000000.0 / trials
  }
     
  def heapSize(base: Long = 0l) = {
    val r = Runtime.getRuntime
    r.totalMemory - r.freeMemory - base
  }
      
  def markHeap() = {
    val r = Runtime.getRuntime
    r.gc()
    Thread.sleep(0l, 50000)
    heapSize()
  }
  
  def timeIt[T](log: Long => Unit)(f: => T) = {
    val t0 = timer()
    val f0 = f
    val t = timer(t0)
    log(t)
    f0
  }

}