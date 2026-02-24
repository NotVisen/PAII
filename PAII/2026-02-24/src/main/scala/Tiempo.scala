


class Tiempo private (val horas: Int, val minutos: Int) {

  override def toString: String = f"$horas%02d:$minutos%02d"
}

object Tiempo {

  def apply(h: Int, m: Int): Tiempo = {

    require(h >= 0  , "El tiempo total no puede ser negativo")
    require(m >= 0  , "El tiempo total no puede ser negativo")

    val totalMinutos = h * 60 + m

    val horasNormalizadas = totalMinutos / 60
    val minutosNormalizados = totalMinutos % 60

    new Tiempo(horasNormalizadas, minutosNormalizados)
  }
}