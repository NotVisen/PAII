// Las funciones pueden quedarse aquí o ir en otro archivo de funciones
def factorialRecursivo(n: Int): Int =
  if (n <= 1) 1 else n * factorialRecursivo(n - 1)

def factorialIterativo(n: Int): Int = {
  var f = 1
  for (i <- 1 to n) f *= i
  f
}

def duplicar(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case a :: r => a :: a :: duplicar(r)
}

def esVacia(l: List[Int]): Boolean = l match {
  case Nil => true
  case _   => false
}

