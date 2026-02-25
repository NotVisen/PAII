def fibonacci (n:Int) : Int = {

  def bucle(i:Int , fibn:Int , fibn1: Int) : Int =
    if(i == n) fibn
    else bucle(i+1, fibn + fibn1 , fibn)

  if(n == 0) 0
  else bucle(1 ,1, 0)

}


def abs (n:Int)  =
  if (n >= 0) n
  else -n

def formatoFib(n:Int) =
  s"El fibonacci de $n es ${fibonacci(n)}"

def formatoAbs(n:Int) =
  s"El valor absoluto de $n es ${abs(n)}"

def formato(n:Int , ms:String , f:Int=>Int) =
  s"El mensaje de $n es ${f(n)}"


fibonacci(6)
formatoAbs(-1)
formatoFib(5)
formato(4, "fibonacci" , fibonacci)


def encontrar(array:Array[Int] , elem:Int ) : Option[Int] = {
  def bucle(i:Int) : Option[Int] = {
    if (i == array.length) None
    else if (array(i) == elem) Some(i)
    else bucle(i + 1)
  }
  bucle(0)
}

// GENERALIZACIÓN DE ESTE BUCLE

def encontrar[A](array:Array[A] , elem:A ) : Option[Int] = {
  def bucle(i:Int) : Option[Int] = {
    if (i == array.length) None
    else if (array(i) == elem) Some(i)
    else bucle(i + 1)
  }
  bucle(0)
}

def encontrarBooleano[A](array:Array[A] , f:A=>Boolean) : Option[Int] = {
  def bucle(i: Int): Option[Int] = {
    if (i == array.length) None
    else if (f(array(i))) Some(i)
    else bucle(i + 1)
  }
  bucle(0)
}

encontrarBooleano(Array(1,2,3,4,5,6,7) , (n:Int) => n%2 == 0) // PUEDO OBVIAR n:Int) =>
val f = (n:Int)=> n%2

import scala.annotation.tailrec

def estaOrdenado[A](array: Array[A], f: (A, A) => Boolean): Boolean = {
  if (array.length <= 1) return true

  def bucle(i: Int): Boolean =
    if (array.length - 1 == i) true
    else
      if (f(array(i), array(i + 1))) false
      else bucle(i + 1)

  bucle(0)
}

estaOrdenado[Int](Array(1, 2, 3, 4), _<=_)


def drop[A]( n :Int , lista:List[A]) : List[A] = {
  require(n>=0)
  if(n==0) lista
  else
    lista match {
      case Nil => Nil
      case a::r => drop(n-1,r)
    }
}

def dropWhile[A](lista:List[A] , f:A=>Boolean) : List[A] = {
  lista match {
    case Nil => Nil
    case a::r =>
      if(f(a)) dropWhile(r,f)
      else lista
  }
}



