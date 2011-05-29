package jp.que.ti.ygame.m.core {

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.{Before, Test}

/**
 *
 * @auther yhj
 */

class MatrixScalaTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Before def init() {}

  @Test def createIntObjectTest() {
    assert(Matrix(1, 2, 3) === MatrixWithInt(1, 2, 3))
    assert(Matrix(1, 2) === MatrixWithInt2D(1, 2))
  }

  @Test def addIntObjectTest() {
    val mtx21_32_0 = MatrixWithInt2D(21, 32)
    assert((Matrix(1, 2) + Matrix(20, 30)) === mtx21_32_0)
    assert(Matrix(20, 30) + (Matrix(1, 2)) === mtx21_32_0)
    assert((Matrix(1, 2).add(Matrix(20, 30))) === mtx21_32_0)

    val mtx21_32_50 = MatrixWithInt(21, 32, 50)
    assert((Matrix(1, 2) + Matrix(20, 30, 50)) === mtx21_32_50)
    assert(Matrix(1, 2).add(Matrix(20, 30, 50)) === mtx21_32_50)
    assert(Matrix(20, 30, 50) + Matrix(1, 2) === mtx21_32_50)
  }

  @Test def productIntObjectTest {
    val mtx40_60_0 = MatrixWithInt2D(40, 60)
    assert(Matrix(20, 30).product(2) === mtx40_60_0)

    val mtx40_60_90 = MatrixWithInt(40, 60, 90)
    assert(Matrix(20, 30, 45).product(2) === mtx40_60_90)
  }

  @Test def productValueAndAddIntObjectTest {
    val mtx120_180_0 = MatrixWithInt2D(120, 180)
    assert(Matrix(20, 30).productValueAndAdd(5) === mtx120_180_0)

    val mtx120_180_240 = MatrixWithInt(120, 180, 240)
    assert(Matrix(20, 30, 40).productValueAndAdd(5) === mtx120_180_240)

    val mtx120_180_240Double = MatrixWithDouble(120, 180, 240)
    assert(Matrix(20, 30, 40).productValueAndAdd(5.0) === mtx120_180_240Double)
    assert(Matrix(20, 30, 40).productValueAndAdd(5L) === mtx120_180_240Double)
    assert(Matrix(20, 30, 40).productValueAndAdd(5f) === mtx120_180_240Double)
  }

  @Test def createDoubleObjectTest() {
    val mtx10_20_30Double = MatrixWithDouble(1.0, 2.0, 3.0)
    assert(Matrix(1.0, 2.0, 3.0) === mtx10_20_30Double)
    assert(Matrix(1.0, 2, 3) === mtx10_20_30Double)
    assert(Matrix(1, 2.0, 3) === mtx10_20_30Double)
    assert(Matrix(1, 2, 3.0) === mtx10_20_30Double)

    assert(Matrix(1.1, 2.2) === MatrixWithDouble2D(1.1, 2.2))

    val mtx10_20_0Double = MatrixWithDouble2D(1.0, 2.0)
    assert(Matrix(1.0, 2.0) === mtx10_20_0Double)
    assert(Matrix(1, 2.0) === mtx10_20_0Double)
    assert(Matrix(1.0, 2) === mtx10_20_0Double)
  }

  @Test def addDouble2DObjectTest() {
    val mtx111_222_0 = MatrixWithDouble2D(11.1, 22.2)
    assert(Matrix(1.0, 2.2) + Matrix(10.1, 20) === mtx111_222_0)
    assert(Matrix(1.0, 2.2).add(Matrix(10.1, 20)) === mtx111_222_0)
    assert(Matrix(10.1, 20) + Matrix(1.0, 2.2) === mtx111_222_0)
    assert(Matrix(10.1, 20).add(Matrix(1.0, 2.2)) === mtx111_222_0)

    assert(Matrix(1.1, 2.2) + Matrix(10, 20) === mtx111_222_0)
    assert(Matrix(1.1, 2.2).add(Matrix(10, 20)) === mtx111_222_0)
    assert(Matrix(10, 20) + Matrix(1.1, 2.2) === mtx111_222_0)
    assert(Matrix(10, 20).add(Matrix(1.1, 2.2)) === mtx111_222_0)
  }

  @Test def addDouble3DObjectTest() {
    val mtx111_222_333 = MatrixWithDouble(11.1, 22.2, 33.3)
    assert(Matrix(1.0, 2.2, 3.3) + Matrix(10.1, 20.0, 30.0) === mtx111_222_333)
    assert(Matrix(10.1, 20.0, 30.0) + Matrix(1.0, 2.2, 3.3) === mtx111_222_333)
    assert(Matrix(1.0, 2.2, 3.3).add(Matrix(10.1, 20.0, 30.0)) === mtx111_222_333)
    assert(Matrix(10.1, 20.0, 30.0).add(Matrix(1.0, 2.2, 3.3)) === mtx111_222_333)

    assert(Matrix(1.1, 2.2, 3.3) + Matrix(10, 20, 30) === mtx111_222_333)
    assert(Matrix(10, 20, 30) + Matrix(1.1, 2.2, 3.3) === mtx111_222_333)
    assert(Matrix(1.1, 2.2, 3.3).add(Matrix(10, 20, 30)) === mtx111_222_333)
    assert(Matrix(10, 20, 30).add(Matrix(1.1, 2.2, 3.3)) === mtx111_222_333)

    assert(Matrix(1.1, 2.2, 33.3) + Matrix(10, 20) === mtx111_222_333)
    assert(Matrix(10, 20) + Matrix(1.1, 2.2, 33.3) === mtx111_222_333)
    assert(Matrix(1.1, 2.2, 33.3).add(Matrix(10, 20)) === mtx111_222_333)
    assert(Matrix(10, 20).add(Matrix(1.1, 2.2, 33.3)) === mtx111_222_333)

    assert(Matrix(1.1, 2.2, 33.3) + Matrix(10.0, 20.0) === mtx111_222_333)
    assert(Matrix(10.0, 20.0) + Matrix(1.1, 2.2, 33.3) === mtx111_222_333)
    assert(Matrix(1.1, 2.2, 33.3).add(Matrix(10.0, 20.0)) === mtx111_222_333)
    assert(Matrix(10.0, 20.0).add(Matrix(1.1, 2.2, 33.3)) === mtx111_222_333)
  }

  @Test def productDouble2DObjectTest {
    val mtx400_600_0 = MatrixWithDouble2D(40.0, 60.0)
    assert(Matrix(20.0, 30.0).product(2) === mtx400_600_0)
    assert(Matrix(20.0, 30.0).product(2.0) === mtx400_600_0)
    assert(Matrix(20.0, 30.0).product(2L) === mtx400_600_0)
    assert(Matrix(20.0, 30.0).product(2f) === mtx400_600_0)
  }

  @Test def productDouble3DObjectTest {
    val mtx400_600_900 = MatrixWithDouble(40.0, 60.0, 90.0)
    assert(Matrix(20.0, 30.0, 45.0).product(2) === mtx400_600_900)
    assert(Matrix(20.0, 30.0, 45.0).product(2.0) === mtx400_600_900)
    assert(Matrix(20.0, 30.0, 45.0).product(2L) === mtx400_600_900)
    assert(Matrix(20.0, 30.0, 45.0).product(2f) === mtx400_600_900)
  }

  @Test def productValueAndAddDouble2DObjectTest {
    val mtx1200_1800_0 = MatrixWithDouble2D(120.0, 180.0)
    assert(Matrix(20.0, 30.0).productValueAndAdd(5) === mtx1200_1800_0)
    assert(Matrix(20.0, 30.0).productValueAndAdd(5.0) === mtx1200_1800_0)
    assert(Matrix(20.0, 30.0).productValueAndAdd(5L) === mtx1200_1800_0)
    assert(Matrix(20.0, 30.0).productValueAndAdd(5f) === mtx1200_1800_0)
  }

  @Test def productValueAndAddDouble3DObjectTest {
    val mtx1200_1800_2400 = MatrixWithDouble(120.0, 180.0, 240.0)
    assert(Matrix(20.0, 30.0, 40.0).productValueAndAdd(5) === mtx1200_1800_2400)
    assert(Matrix(20.0, 30.0, 40.0).productValueAndAdd(5.0) === mtx1200_1800_2400)
    assert(Matrix(20.0, 30.0, 40.0).productValueAndAdd(5L) === mtx1200_1800_2400)
    assert(Matrix(20.0, 30.0, 40.0).productValueAndAdd(5f) === mtx1200_1800_2400)
  }

  @Test def addTest {
    var m1: Matrix = Matrix(1.1, 2.2, 3.3)
    var m2: Matrix = Matrix(10.01, 20.02, 30.03)
    assert(m1 + m2 === MatrixWithDouble(11.11, 22.22, 33.33))

    m2 = Matrix(10, 20, 30)
    assert(m1 + m2 === MatrixWithDouble(11.1, 22.2, 33.3))
    assert(m2 + m1 === MatrixWithDouble(11.1, 22.2, 33.3))

    m2 = Matrix(10, 20)
    assert(m1 + m2 === MatrixWithDouble(11.1, 22.2, 3.3))
    assert(m2 + m1 === MatrixWithDouble(11.1, 22.2, 3.3))

    m2 = Matrix(10.01, 20.02)
    assert(m1 + m2 === MatrixWithDouble(11.11, 22.22, 3.3))
    assert(m2 + m1 === MatrixWithDouble(11.11, 22.22, 3.3))

    //

    m1 = Matrix(1, 2, 3)
    m2 = Matrix(10, 20, 30)
    assert(m1 + m2 === MatrixWithInt(11, 22, 33))

    m2 = Matrix(10.1, 20.2, 30.3)
    assert(m1 + m2 === MatrixWithDouble(11.1, 22.2, 33.3))
    assert(m2 + m1 === MatrixWithDouble(11.1, 22.2, 33.3))

    m2 = Matrix(10, 20)
    assert(m1 + m2 === MatrixWithInt(11, 22, 3))
    assert(m2 + m1 === MatrixWithInt(11, 22, 3))

    m2 = Matrix(10.1, 20.2)
    assert(m1 + m2 === MatrixWithDouble(11.1, 22.2, 3))
    assert(m2 + m1 === MatrixWithDouble(11.1, 22.2, 3))

    //

    m1 = Matrix(1, 2)
    m2 = Matrix(10, 20)
    assert(m1 + m2 === MatrixWithInt2D(11, 22))

    m2 = Matrix(10.1, 20.2)
    assert(m1 + m2 === MatrixWithDouble2D(11.1, 22.2))
    assert(m2 + m1 === MatrixWithDouble2D(11.1, 22.2))

    //
    m1 = Matrix(1.1, 2.2)
    m2 = Matrix(10.01, 20.02, 0.3)
    assert(m1 + m2 === MatrixWithDouble(11.11, 22.22, 0.3))
    assert(m2 + m1 === MatrixWithDouble(11.11, 22.22, 0.3))
  }
}

}