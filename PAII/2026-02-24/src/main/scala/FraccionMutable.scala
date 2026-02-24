class FraccionMutable(private var num : Int , private var den : Int) {
  def this(num:Int) =
    this(num,1)

  override def toString()=
    if(den!=1 ) s"$num/$den"
    else s"$num"

  def producto(fraccion: FraccionMutable): Unit = {
    this.num = this.num * fraccion.num
    this.den = this.den * fraccion.den;
  }
}



