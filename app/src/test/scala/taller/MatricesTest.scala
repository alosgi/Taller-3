package taller

import org.scalatest.funsuite.AnyFunSuite

class MatricesTest extends AnyFunSuite {

  val taller = new Matrices()
  type Matriz = Vector[Vector[Int]]

  // Tests para sumMatriz
  test("sumMatriz suma correctamente dos matrices cuadradas") {
    val m1 = Vector(Vector(1, 2), Vector(3, 4))
    val m2 = Vector(Vector(5, 6), Vector(7, 8))
    val esperado = Vector(Vector(6, 8), Vector(10, 12))
    assert(taller.sumMatriz(m1, m2) == esperado)
  }

  test("sumMatriz con matrices de diferentes valores") {
    val m1 = Vector(Vector(0, -1), Vector(-3, 4))
    val m2 = Vector(Vector(1, 1), Vector(3, -4))
    val esperado = Vector(Vector(1, 0), Vector(0, 0))
    assert(taller.sumMatriz(m1, m2) == esperado)
  }

  test("sumMatriz con matrices cero") {
    val m1 = Vector(Vector(0, 0), Vector(0, 0))
    val m2 = Vector(Vector(0, 0), Vector(0, 0))
    val esperado = Vector(Vector(0, 0), Vector(0, 0))
    assert(taller.sumMatriz(m1, m2) == esperado)
  }

  test("sumMatriz con matrices de 1x1") {
    val m1 = Vector(Vector(3))
    val m2 = Vector(Vector(-3))
    val esperado = Vector(Vector(0))
    assert(taller.sumMatriz(m1, m2) == esperado)
  }

  // Tests para multMatrizRec
  test("multMatrizRec multiplica correctamente dos matrices cuadradas") {
    val m1 = Vector(Vector(1, 2), Vector(3, 4))
    val m2 = Vector(Vector(5, 6), Vector(7, 8))
    val esperado = Vector(Vector(19, 22), Vector(43, 50))
    assert(taller.multMatrizRec(m1, m2) == esperado)
  }

  test("multMatrizRec con matrices de 1x1") {
    val m1 = Vector(Vector(2))
    val m2 = Vector(Vector(3))
    val esperado = Vector(Vector(6))
    assert(taller.multMatrizRec(m1, m2) == esperado)
  }

  test("multMatrizRec con matriz identidad") {
    val m1 = Vector(Vector(1, 0), Vector(0, 1))
    val m2 = Vector(Vector(5, 6), Vector(7, 8))
    val esperado = m2
    assert(taller.multMatrizRec(m1, m2) == esperado)
  }

  test("multMatrizRec con matrices cero") {
    val m1 = Vector(Vector(0, 0), Vector(0, 0))
    val m2 = Vector(Vector(0, 0), Vector(0, 0))
    val esperado = Vector(Vector(0, 0), Vector(0, 0))
    assert(taller.multMatrizRec(m1, m2) == esperado)
  }


  // Tests para multMatrizRecPar
  test("multMatrizRecPar multiplica correctamente matrices pequeñas") {
    val m1 = Vector(Vector(1, 2), Vector(3, 4))
    val m2 = Vector(Vector(5, 6), Vector(7, 8))
    val esperado = Vector(Vector(19, 22), Vector(43, 50))
    assert(taller.multMatrizRecPar(m1, m2) == esperado)
  }

  test("restaMatriz resta correctamente dos matrices cuadradas") {
  val m1 = Vector(Vector(5, 6), Vector(7, 8))
  val m2 = Vector(Vector(1, 2), Vector(3, 4))
  val esperado = Vector(Vector(4, 4), Vector(4, 4))
  assert(taller.resMatriz(m1, m2) == esperado)
}

test("restaMatriz con matrices de valores negativos") {
  val m1 = Vector(Vector(-5, -6), Vector(-7, -8))
  val m2 = Vector(Vector(-1, -2), Vector(-3, -4))
  val esperado = Vector(Vector(-4, -4), Vector(-4, -4))
  assert(taller.resMatriz(m1, m2) == esperado)
}

test("restaMatriz con matrices de ceros") {
  val m1 = Vector(Vector(0, 0), Vector(0, 0))
  val m2 = Vector(Vector(0, 0), Vector(0, 0))
  val esperado = Vector(Vector(0, 0), Vector(0, 0))
  assert(taller.resMatriz(m1, m2) == esperado)
}


test("restaMatriz con matrices de 1x1") {
  val m1 = Vector(Vector(10))
  val m2 = Vector(Vector(3))
  val esperado = Vector(Vector(7))
  assert(taller.resMatriz(m1, m2) == esperado)
}

test("multStrassen multiplica correctamente matrices cuadradas") {
  val m1 = Vector(Vector(1, 2), Vector(3, 4))
  val m2 = Vector(Vector(5, 6), Vector(7, 8))
  val esperado = Vector(Vector(19, 22), Vector(43, 50))
  assert(taller.multStrassen(m1, m2) == esperado)
}

test("multStrassen con matrices identidad") {
  val m1 = Vector(Vector(1, 0), Vector(0, 1))
  val m2 = Vector(Vector(5, 6), Vector(7, 8))
  val esperado = m2
  assert(taller.multStrassen(m1, m2) == esperado)
}

test("multStrassen con matrices de 1x1") {
  val m1 = Vector(Vector(2))
  val m2 = Vector(Vector(3))
  val esperado = Vector(Vector(6))
  assert(taller.multStrassen(m1, m2) == esperado)
}

test("multStrassen con matrices de ceros") {
  val m1 = Vector(Vector(0, 0), Vector(0, 0))
  val m2 = Vector(Vector(0, 0), Vector(0, 0))
  val esperado = Vector(Vector(0, 0), Vector(0, 0))
  assert(taller.multStrassen(m1, m2) == esperado)
}


test("prodPunto calcula correctamente el producto punto") {
  val v1 = Vector(1, 2, 3)
  val v2 = Vector(4, 5, 6)
  val esperado = 32 // 1*4 + 2*5 + 3*6
  assert(taller.prodEscalar(v1, v2) == esperado)
}

test("prodPunto con vectores de ceros") {
  val v1 = Vector(0, 0, 0)
  val v2 = Vector(0, 0, 0)
  val esperado = 0
  assert(taller.prodEscalar(v1, v2) == esperado)
}

test("prodPunto con vectores negativos") {
  val v1 = Vector(-1, -2, -3)
  val v2 = Vector(4, 5, 6)
  val esperado = -32 // -1*4 + -2*5 + -3*6
  assert(taller.prodEscalar(v1, v2) == esperado)
}

test("prodPunto con un vector vacío") {
  val v1 = Vector[Int]()
  val v2 = Vector[Int]()
  val esperado = 0
  assert(taller.prodEscalar(v1, v2) == esperado)
}

test("prodPunto con vectores de diferentes tamaños (debería fallar)") {
  val v1 = Vector(1, 2)
  val v2 = Vector(3, 4, 5)
  assertThrows[IllegalArgumentException] {
    taller.prodEscalar(v1, v2)
  }
}

test("multStrassenPar multiplica correctamente matrices cuadradas") {
  val m1 = Vector(Vector(1, 2), Vector(3, 4))
  val m2 = Vector(Vector(5, 6), Vector(7, 8))
  val esperado = Vector(Vector(19, 22), Vector(43, 50))
  assert(taller.multStrassenPar(m1, m2) == esperado)
}

test("multStrassenPar con matrices identidad") {
  val m1 = Vector(Vector(1, 0), Vector(0, 1))
  val m2 = Vector(Vector(5, 6), Vector(7, 8))
  val esperado = m2
  assert(taller.multStrassenPar(m1, m2) == esperado)
}

test("multStrassenPar con matrices de ceros") {
  val m1 = Vector(Vector(0, 0), Vector(0, 0))
  val m2 = Vector(Vector(0, 0), Vector(0, 0))
  val esperado = Vector(Vector(0, 0), Vector(0, 0))
  assert(taller.multStrassenPar(m1, m2) == esperado)
}

test("multStrassenPar con matrices de 1x1") {
  val m1 = Vector(Vector(2))
  val m2 = Vector(Vector(3))
  val esperado = Vector(Vector(6))
  assert(taller.multStrassenPar(m1, m2) == esperado)
}

test("multStrassenPar con matrices grandes cuadradas") {
  val size = 4
  val m1 = Vector.tabulate(size, size)((i, j) => i + j + 1)
  val m2 = Vector.tabulate(size, size)((i, j) => i - j + 1)
  val esperado = taller.multMatriz(m1, m2) // Validamos contra el resultado de la versión no paralela
  assert(taller.multStrassenPar(m1, m2) == esperado)
}



}
