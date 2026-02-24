class Persona (nombre: String , dni : Int) {
  def this(nombre : String) =
    this(nombre, 300)

  def this() =
    this("sin nombre")

  def getDni: Int = dni
  def getNombre: String = nombre

  override def toString : String =
    s"($nombre,$dni)"

}
