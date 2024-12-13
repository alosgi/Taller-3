package taller
import common.{parallel, task}
import scala.collection.parallel.immutable.ParVector
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
class Matrices {

    //Definicion de tipos
    val random = new Random()
    type Matriz =  Vector[Vector[Int]]

    def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long, long) {random.nextInt(vals)}
    v
  }

  def prodEscalar(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({case (i,j) => (i*j)}).sum
  }

  def transpuesta (m: Matriz): Matriz = {
    val l =m.length
    Vector.tabulate(l,l)((i,j) => m(j)(i))
  }

  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val m2t = transpuesta(m2)
    Vector.tabulate(m1.length, m2.length) { case (i, j) => prodEscalar(m1(i), m2t(j)) }
  }


  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val m2t = transpuesta(m2)
    val limite = 2

    def auxPar(inf: Int, sup: Int): Matriz = {
      if (sup - inf < limite) Vector.tabulate(1, m2.length) { case (i, j) => prodEscalar(m1(inf), m2t(j)) }
      else {
        val m = inf + (sup - inf) / 2
        val (x, y) = parallel(auxPar(inf, m), auxPar(m, sup))
        x ++ y
      }
    }
      auxPar(0, m1.length)
  }


  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz ={
        Vector.tabulate(l, l)((a, b) => m(a + i)(b + j))
    }


  def sumMatriz(m1: Matriz, m2: Matriz): Matriz =
  {
    Vector.tabulate(m1.length, m1.length)((a, b) => m1(a)(b) + m2(a)(b))
  }

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz =
  {
    val n = m1.length

    if (n == 1)
    {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
    else
    {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22}
    }
  }


  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz =
  {
    val n = m1.length

    if(n == 8)
    {
      multMatrizRec(m1, m2)
    }
    else
    {
      if (n == 1) {
        Vector(Vector(m1(0)(0) * m2(0)(0)))
      }
      else
      {
        val m = n / 2

        val a11 = subMatriz(m1, 0, 0, m)
        val a12 = subMatriz(m1, 0, m, m)
        val a21 = subMatriz(m1, m, 0, m)
        val a22 = subMatriz(m1, m, m, m)

        val b11 = subMatriz(m2, 0, 0, m)
        val b12 = subMatriz(m2, 0, m, m)
        val b21 = subMatriz(m2, m, 0, m)
        val b22 = subMatriz(m2, m, m, m)

        val c11 = task(sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21)))
        val c12 = task(sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22)))
        val c21 = task(sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21)))
        val c22 = task(sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22)))

        c11.join().zip(c12.join()).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.join().zip(c22.join()).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }
      }
    }
  }

  def resMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((a, b) => m1(a)(b) - m2(a)(b))
  }

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
    else {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val s1 = resMatriz(b12, b22)
      val s2 = sumMatriz(a11,a12)
      val s3 = sumMatriz(a21,a22)
      val s4 = resMatriz(b21,b11)
      val s5 = sumMatriz(a11,a22)
      val s6 = sumMatriz(b11,b22)
      val s7 = resMatriz(a12,a22)
      val s8 = sumMatriz(b21,b22)
      val s9 = resMatriz(a11,a21)
      val s10 = sumMatriz(b11,b12)

      val p1 = multStrassen(a11,s1)
      val p2 = multStrassen(s2,b22)
      val p3 = multStrassen(s3,b11)
      val p4 = multStrassen(a22,s4)
      val p5 = multStrassen(s5,s6)
      val p6 = multStrassen(s7,s8)
      val p7 = multStrassen(s9,s10)

      val c11 = resMatriz(sumMatriz(p6,sumMatriz(p5,p4)),p2)
      val c12 = sumMatriz(p1,p2)
      val c21 = sumMatriz(p3,p4)
      val c22 = resMatriz(sumMatriz(p5,p1), sumMatriz(p3,p7))

      c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22}
    }
  }


  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
    else {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val s1 = resMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = resMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = resMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = resMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      val p1 = task(multStrassenPar(a11, s1))
      val p2 = task(multStrassenPar(s2, b22))
      val p3 = task(multStrassenPar(s3, b11))
      val p4 = task(multStrassenPar(a22, s4))
      val p5 = task(multStrassenPar(s5, s6))
      val p6 = task(multStrassenPar(s7, s8))
      val p7 = task(multStrassenPar(s9, s10))

      val c11 = task(resMatriz(sumMatriz(p6.join(), sumMatriz(p5.join(), p4.join())), p2.join()))
      val c12 = task(sumMatriz(p1.join(), p2.join()))
      val c21 = task(sumMatriz(p3.join(), p4.join()))
      val c22 = task(resMatriz(sumMatriz(p5.join(), p1.join()), sumMatriz(p3.join(), p7.join())))

      c11.join().zip(c12.join()).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.join().zip(c22.join()).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }
    }
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
        (v1 zip v2).map { case (i, j) => i * j }.sum
    }


  
}


