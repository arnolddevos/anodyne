package anodyne

class CharSeqView( base: CharSequence, offset: Int, val length: Int) extends CharSequence {
  def this(base: CharSequence) = this(base, 0, base.length)
  
  def charAt(index: Int): Char = {
    if( 0 <= index && index < length)
      base.charAt(index + offset)
    else
      throw new IndexOutOfBoundsException
  }
  
  def subSequence(start: Int, end: Int): CharSequence = {
    if(0 <= start && start <= end && end <= length) {
      if(start == 0 && end == length)
        this
      else if(start == end)
        new CharSeqView("", 0, 0)
      else
        new CharSeqView(base, start+offset, end-start)
    }
    else
      throw new IndexOutOfBoundsException
  }

  override def toString: String = new java.lang.StringBuilder(this).toString
}
