package anodyne

import java.io.FilterInputStream
import java.io.InputStream
import java.io.IOException
import scala.io.{Source, Codec}

object BoundedStream {
  def readBounded(is: => InputStream, expect: Long, limit: Long)( implicit codec: Codec): CharSequence = {
    if( expect > limit )
      throw new IOException("advertised stream length is greater than " + limit)
    
    val readLimit = if( expect >= 0 ) expect else limit
    val ls = new BoundedStream(is, readLimit)
    val sb = new StringBuilder
    
    try {
     Source.fromInputStream(ls)(codec).addString(sb)
    }
    finally {
      ls.close()
    }
    
    sb: CharSequence
  }
}

class BoundedStream(is: InputStream, limit: Long) extends FilterInputStream(is) {
  private var seen = 0l
  
  def count = seen
  
  private def check(n: Int): Int = {
    if( n > 0) {
      seen += n
      if( seen > limit)
        throw new IOException("length of input stream is greater than " + limit)
    }
    n
  }
  
  override def read(): Int = {
    val b = read()
    if( b >= 0 )
      check(1)
    b
  }
  
  override def read(bs: Array[Byte], off: Int, len: Int): Int = {
    check { super.read(bs, off, len) }
  }

  override def read(bs: Array[Byte]): Int = {
    check { super.read(bs) }
  }
}
