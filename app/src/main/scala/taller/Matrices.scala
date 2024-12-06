package taller
import scala.util.Random

package object Matrices{

    val random = new Random()
    type Matriz = Vector[Vector[Int]]

    def matrizAlAzar(long: Int, vals: Int): Matriz = {
        val v = Vector.fill(long,long){random.nextInt(vals)}
    }

    def prodpunto(v1: Vector[Int], v2: Vector[Int]): Int={
        (v1 zip v2).map({case(1,j)=>(i*j)}).sum
    }

}