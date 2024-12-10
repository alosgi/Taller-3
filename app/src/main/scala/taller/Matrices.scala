package taller
import scala.util.Random

package object Matrices{

    val random = new Random()
    type Matriz = Vector[Vector[Int]]

    def matrizAlAzar(long: Int, vals: Int): Matriz = {
        val v = Vector.fill(long,long){random.nextInt(vals)}
    }

    def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int={
        (v1 zip v2).map({case(1,j)=>(i*j)}).sum
    }

    def transpuesta (m: Matriz): Matriz ={
        val l=m.length
        Vector.tabulate(l,l)((i,j)=>m(j)(i))
    }

    def mulMatriz(m1: Matriz, m2: Matriz): Matriz = {
        val m2t = (transpuesta(m2))
        Vector.tabulate(m1.length, m2.length){
            case (i,j)=> prodPunto(m1(i), m2t(j))}
    }

}