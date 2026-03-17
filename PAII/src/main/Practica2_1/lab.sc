
def primeFactors(n : Int) : List[Int] = {

  def bucle(n : Int, divisor : Int , acc : List[Int]) : List[Int] = {
    if(n == 1){
      acc.reverse
    } else if (n % divisor == 0) {
      bucle(n/divisor  , divisor  , divisor::acc)
    } else {
      bucle(n , divisor + 1, acc)
    }
  }

  bucle(n,2,Nil)
}

println(primeFactors(60))
println(primeFactors(97))
println(primeFactors(84))


def binarySearch(arr: Array[Int], elt: Int): Option[Int] = {

  def bucle(bajo: Int, alto: Int): Option[Int] = {
    if (bajo > alto) None
    else {
      val mid = (bajo + alto) / 2

      if (arr(mid) == elt) Some(mid)
      else if (arr(mid) < elt)
        bucle(mid + 1, alto)
      else
        bucle(bajo, mid - 1)
    }
  }
  bucle(0, arr.length - 1)
}

val arr = Array(1, 3, 5, 7, 9, 11)
println(binarySearch(arr, 5))
println(binarySearch(arr, 10))


def unzipInt(lista: List[(Int, Int)]): (List[Int], List[Int]) = {
  def recursiveTask( resto: List[(Int, Int)], accA: List[Int], accB: List[Int]) : (List[Int], List[Int]) = {
    if (resto.isEmpty) (accA.reverse , accB.reverse)
    else {
      val (a,b) = resto.head
      recursiveTask(resto.tail , a::accA , b:: accB)
    }
  }
  recursiveTask(lista,Nil,Nil)
}

def unzipIntPM(lista: List[(Int, Int)]): (List[Int], List[Int]) = {
  def patternMatching(resto : List[(Int,Int)] , accA: List[Int], accB: List[Int]) : (List[Int], List[Int]) = {
    resto match {
      case Nil => (accA.reverse , accB.reverse)
      case (a,b) :: tail => patternMatching(tail , a::accA , b::accB )
    }
  }
  patternMatching(lista ,Nil,Nil)
}


def unzip[A, B]( lista : List[(A,B)] ) : (List[A] , List[B]) = {
  def recursiveTask(resto : List[(A,B)] , accA: List[A], accB: List[B]): (List[A], List[B]) = {
    if(resto.isEmpty) (accA.reverse , accB.reverse)
    else {
      val (a,b) = resto.head
      recursiveTask(resto.tail , a::accA , b::accB)
    }
  }
  recursiveTask(lista, Nil, Nil)
}

unzip(List((10, 'a'), (20, 'b'), (30, 'c')))

def unzipPM[A, B]( lista : List[(A,B)] ) : (List[A] , List[B]) = {
  def recursiveTask(resto : List[(A,B)] , accA: List[A], accB: List[B]): (List[A], List[B]) = {
    resto match {
      case Nil => (accA.reverse , accB.reverse)
      case (a,b) :: tail => recursiveTask(tail , a::accA , b::accB)
    }
  }
  recursiveTask(lista, Nil, Nil)
}

unzipPM(List((10, 'a'), (20, 'b'), (30, 'c')))


def filter [A]( lista : List[A] ,  f : A => Boolean) : List[A] = {
  def patternMatchingCatch( acc : List[A] , list : List[A]) : List[A] = {
    list match {
      case Nil => acc.reverse
      case x :: tail if f(x) =>
        patternMatchingCatch(x :: acc, tail)
      case _ :: tail =>
        patternMatchingCatch(acc, tail)
    }
  }
  patternMatchingCatch(Nil , lista)
}

println(filter(List(1,2,3,4,5), (x: Int) => x % 2 == 0))

def map[A,B](lista : List[A] , f : A => B) : List[B] = {
  def patternMatchingCatch(acc : List[B] , list : List[A]) : List[B] = {
    list match {
      case Nil => acc.reverse
      case x :: tail => patternMatchingCatch( f(x) :: acc , tail)
    }
  }
  patternMatchingCatch(Nil , lista)
}

println(map(List(1, 2, 3, 4, 5), (x: Int) => x * 2))



def groupByBasic[A,B] (lista : List[A] , f : A => Boolean) : Map[Boolean, List[A]] = {
  def patternMatching( accT : List[A] , accF : List[A] , list : List[A]) : Map[Boolean, List[A]] = {
    list match {
      case Nil =>  Map(
        true -> accT.reverse ,
        false -> accF.reverse
      )
      case x :: tail if f(x) => patternMatching(x ::accT , accF , tail)
      case x :: tail if !f(x) => patternMatching(accT , x :: accF , tail)
    }
  }
  patternMatching(Nil , Nil,lista)
}


println(groupByBasic(List(1,2,3,4,5), (x:Int) => x%2 == 0))



def groupBy[A, B](lista: List[A], f: A => B): Map[B, List[A]] = {

  def patternMatching(resto : List[A] , acc : Map[B , List[A]]) : Map[B , List[A]] = {
    resto match {
      case Nil => acc;
      case x :: tail  =>
        val clave: B = f(x)
        val listaPrevia = acc.getOrElse(clave , Nil)
        val nuevaLista = x :: listaPrevia

        patternMatching(tail , acc + (clave -> nuevaLista))
    }
  }
  patternMatching(lista, Map.empty[B,List[A]])
}

println(groupBy(List(1,2,3,4,5), (x:Int) => x%2 == 0))


def reduce [A]( list : List[A] , f : (A,A) => A) : A = {

  def bucle(lista : List[A]) : A = {
    lista match {
      case Nil => throw new UnsupportedOperationException("Lista Vacía")
      case a :: Nil => lista.head
      case a :: b :: tail => bucle (f(a,b) :: tail)
    }
  }

  bucle(list)
}

println(reduce(List(1,2,3,4,5), (a: Int, b: Int) => a + b))


def subsets[A](s: Set[A]): Set[Set[A]] = {

  def recursiveTask(elementos: List[A], acc: Set[Set[A]]): Set[Set[A]] = {
    elementos match {
      case Nil => acc
      case x :: tail =>
        val nuevos = acc.map( acc => acc + x)
        recursiveTask(tail , acc++nuevos)
    }
  }
  recursiveTask(s.toList, Set(Set.empty[A]))
}

println(subsets(Set(1, 2, 3)))




