package taller
import scala.util.Random

package object Matrices{

    val random = new Random()
    type Matriz = Vector[Vector[Int]]

    def matrizAlAzar(long: Int, vals: Int): Matriz = {
        val v = Vector.fill(long,long){random.nextInt(vals)}
    }

}