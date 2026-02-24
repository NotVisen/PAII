// El objeto companion (la "fábrica")
object Fraccion {
  // Método apply para crear objetos sin 'new'
  def apply(num: Int, den: Int): Fraccion = {
    // Función interna para calcular el Máximo Común Divisor
    def mcd(a: Int, b: Int): Int = if (b == 0) a.abs else mcd(b, a % b)
    val comun = mcd(num, den)
    // Devolvemos una nueva instancia normalizada
    new Fraccion(num / comun, den / comun)
  }
}

// La clase con constructor privado
class Fraccion private (num: Int, den: Int) {
  // Validación de seguridad
  require(den != 0, "El denominador no puede ser 0")

  // Getters para acceder a los valores inmutables
  def getNum: Int = num
  def getDen: Int = den

  // Operación que devuelve un objeto NUEVO (Inmutabilidad)
  def *(otra: Fraccion): Fraccion =
    Fraccion(num * otra.getNum, den * otra.getDen)

  override def toString: String =
    if (den == 1) s"$num" else s"$num/$den"
}