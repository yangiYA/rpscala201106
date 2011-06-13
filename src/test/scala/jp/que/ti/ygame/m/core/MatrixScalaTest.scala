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
    val actualInt = Matrix(1, 2, 3)
    assert(actualInt === MatrixWithInt(1, 2, 3))
    assert(actualInt.isInstanceOf[MatrixWithInt], actualInt + ",の型をテスト")

    val actual2d = Matrix(1, 2)
    assert(actual2d === MatrixWithInt2D(1, 2))
    assert(actual2d.isInstanceOf[MatrixWithInt2D], actual2d + ",の型をテスト")
  }

  @Test def addIntObjectTest() {
    val mtx21_32_0 = MatrixWithInt2D(21, 32)
    assert((Matrix(1, 2) + Matrix(20, 30)) === mtx21_32_0)
    assert((Matrix(20, 30) + Matrix(1, 2)) === mtx21_32_0)
    assert((Matrix(1, 2).add(Matrix(20, 30))) === mtx21_32_0)
    assert((Matrix(1, 2) + Matrix(20, 30)).isInstanceOf[MatrixWithInt2D] === true)

    val mtx21_32_50 = MatrixWithInt(21, 32, 50)
    assert((Matrix(1, 2) + Matrix(20, 30, 50)) === mtx21_32_50)
    assert(Matrix(1, 2).add(Matrix(20, 30, 50)) === mtx21_32_50)
    assert(Matrix(20, 30, 50) + Matrix(1, 2) === mtx21_32_50)
    assert((Matrix(20, 30, 50) + Matrix(1, 2)).isInstanceOf[MatrixWithInt] === true)
  }

  @Test def productIntObjectTest {
    val mtx40_60_0 = MatrixWithInt2D(40, 60)
    val actualInt2d = Matrix(20, 30).product(2)
    assert(actualInt2d === mtx40_60_0)
    assert(actualInt2d.isInstanceOf[MatrixWithInt2D], actualInt2d + ",の型をテスト")

    val mtx40_60_90 = MatrixWithInt(40, 60, 90)
    val actualInt = Matrix(20, 30, 45).product(2)
    assert(actualInt === mtx40_60_90)
    assert(actualInt.isInstanceOf[MatrixWithInt], actualInt + ",の型をテスト")
  }

  @Test def productValueAndAddIntObjectTest {
    val mtx120_180_0 = MatrixWithInt2D(120, 180)
    val actualInt2d = Matrix(20, 30).productValueAndAdd(5)
    assert(Matrix(20, 30).productValueAndAdd(5) === mtx120_180_0)
    assert(actualInt2d.isInstanceOf[MatrixWithInt2D], actualInt2d + ",の型をテスト")

    val mtx120_180_240 = MatrixWithInt(120, 180, 240)
    val actualInt = Matrix(20, 30, 40).productValueAndAdd(5)
    assert(actualInt === mtx120_180_240)
    assert(actualInt.isInstanceOf[MatrixWithInt], actualInt + ",の型をテスト")

    val mtx120_180_240Double = MatrixWithDouble(120, 180, 240)
    assert(Matrix(20, 30, 40).productValueAndAdd(5.0) === mtx120_180_240Double)
    assert(Matrix(20, 30, 40).productValueAndAdd(5L) === mtx120_180_240Double)
    assert(Matrix(20, 30, 40).productValueAndAdd(5f) === mtx120_180_240Double)
    assert((Matrix(20, 30, 40).productValueAndAdd(5.0)).isInstanceOf[MatrixWithDouble] === true)
    assert((Matrix(20, 30, 40).productValueAndAdd(5L)).isInstanceOf[MatrixWithDouble] === true)
    assert((Matrix(20, 30, 40).productValueAndAdd(5f)).isInstanceOf[MatrixWithDouble] === true)
  }

  @Test def createDoubleObjectTest() {
    val mtx10_20_30Double = MatrixWithDouble(1.0, 2.0, 3.0)
    assert(Matrix(1.0, 2.0, 3.0) === mtx10_20_30Double)
    assert(Matrix(1.0, 2, 3) === mtx10_20_30Double)
    assert(Matrix(1, 2.0, 3) === mtx10_20_30Double)
    assert(Matrix(1, 2, 3.0) === mtx10_20_30Double)

    assert(Matrix(1.1, 2.2, 3.3) != Matrix(1, 2, 3)) //少数点以下が切り捨てられた等号判定になっていないかのテスト

    assert(Matrix(1.1, 2.2) === MatrixWithDouble2D(1.1, 2.2))
    assert(Matrix(1.1, 2.2) != Matrix(1, 2)) //少数点以下が切り捨てられた等号判定になっていないかのテスト


    val mtx10_20_0Double = MatrixWithDouble2D(1.0, 2.0)
    assert(Matrix(1.0, 2.0) === mtx10_20_0Double)
    assert(Matrix(1, 2.0) === mtx10_20_0Double)
    assert(Matrix(1.0, 2) === mtx10_20_0Double)

    //メモリを無駄遣いしない型で生成されるかのテスト
    assert((Matrix(1.0, 2.0)).isInstanceOf[MatrixWithDouble2D] === true)
    assert((Matrix(1, 2.0)).isInstanceOf[MatrixWithDouble2D] === true)
    assert((Matrix(1.0, 2)).isInstanceOf[MatrixWithDouble2D] === true)
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

    //メモリを無駄遣いしない型で生成されるかのテスト
    assert((Matrix(1.0, 2.2) + Matrix(10.1, 20)).isInstanceOf[MatrixWithDouble2D] === true)
    assert((Matrix(1.1, 2.2) + Matrix(10, 20)).isInstanceOf[MatrixWithDouble2D] === true)
    assert(Matrix(10, 20).add(Matrix(1.1, 2.2)).isInstanceOf[MatrixWithDouble2D] === true)
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

    //メモリを無駄遣いしない型で生成されるかのテスト
    assert((Matrix(20.0, 30.0).product(2)).isInstanceOf[MatrixWithDouble2D] === true)
    assert((Matrix(20.0, 30.0).product(2.0)).isInstanceOf[MatrixWithDouble2D] === true)
    assert((Matrix(20.0, 30.0).product(2L)).isInstanceOf[MatrixWithDouble2D] === true)
    assert((Matrix(20.0, 30.0).product(2f)).isInstanceOf[MatrixWithDouble2D] === true)
  }

  @Test def productDouble3DObjectTest {
    val mtx400_600_900 = MatrixWithDouble(40.2, 60.2, 90.2)
    assert(Matrix(20.1, 30.1, 45.1).product(2) === mtx400_600_900)
    assert(Matrix(20.1, 30.1, 45.1).product(2.0) === mtx400_600_900)
    assert(Matrix(20.1, 30.1, 45.1).product(2L) === mtx400_600_900)
    assert(Matrix(20.1, 30.1, 45.1).product(2f) === mtx400_600_900)
  }

  @Test def productValueAndAddDouble2DObjectTest {
    val mtx120_180_0 = MatrixWithDouble2D(120.6, 180.6)
    assert(Matrix(20.1, 30.1).productValueAndAdd(5) === mtx120_180_0)
    assert(Matrix(20.1, 30.1).productValueAndAdd(5.0) === mtx120_180_0)
    assert(Matrix(20.1, 30.1).productValueAndAdd(5L) === mtx120_180_0)
    assert(Matrix(20.1, 30.1).productValueAndAdd(5f) === mtx120_180_0)
  }

  @Test def productValueAndAddDouble3DObjectTest {
    val mtx120_180_240 = MatrixWithDouble(120.6, 180.6, 240.6)
    assert(Matrix(20.1, 30.1, 40.1).productValueAndAdd(5) === mtx120_180_240)
    assert(Matrix(20.1, 30.1, 40.1).productValueAndAdd(5.0) === mtx120_180_240)
    assert(Matrix(20.1, 30.1, 40.1).productValueAndAdd(5L) === mtx120_180_240)
    assert(Matrix(20.1, 30.1, 40.1).productValueAndAdd(5f) === mtx120_180_240)
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
    val actualInt = m1 + m2
    assert(actualInt === MatrixWithInt(11, 22, 33))
    assert(actualInt.isInstanceOf[MatrixWithInt], actualInt + ",の型をテスト")


    m2 = Matrix(10.1, 20.2, 30.3)
    assert(m1 + m2 === MatrixWithDouble(11.1, 22.2, 33.3))
    assert(m2 + m1 === MatrixWithDouble(11.1, 22.2, 33.3))

    m2 = Matrix(10, 20)
    assert(m1 + m2 === MatrixWithInt(11, 22, 3))
    assert(m2 + m1 === MatrixWithInt(11, 22, 3))
    assert((m1 + m2).isInstanceOf[MatrixWithInt] === true)

    m2 = Matrix(10.1, 20.2)
    assert(m1 + m2 === MatrixWithDouble(11.1, 22.2, 3))
    assert(m2 + m1 === MatrixWithDouble(11.1, 22.2, 3))

    //

    m1 = Matrix(1, 2)
    m2 = Matrix(10, 20)
    val actualInt2d = m1 + m2
    assert(actualInt2d === MatrixWithInt2D(11, 22))
    assert(actualInt2d.isInstanceOf[MatrixWithInt2D], actualInt2d + ",の型をテスト")

    m2 = Matrix(10.1, 20.2)
    assert(m1 + m2 === MatrixWithDouble2D(11.1, 22.2))
    assert(m2 + m1 === MatrixWithDouble2D(11.1, 22.2))
    assert((m2 + m1).isInstanceOf[MatrixWithDouble2D] === true)

    //
    m1 = Matrix(1.1, 2.2)
    m2 = Matrix(10.01, 20.02, 0.3)
    assert(m1 + m2 === MatrixWithDouble(11.11, 22.22, 0.3))
    assert(m2 + m1 === MatrixWithDouble(11.11, 22.22, 0.3))
  }

  @Test def hashCodeTest {
    val map = scala.collection.mutable.Map[Matrix, Int]()
    map += Matrix(2, 1) -> 1 //2,1 の組み合わせは全部同じとみなされるはず
    map += Matrix(2.0f, 1.0f) -> 2 //2,1 の組み合わせは全部同じとみなされるはず
    map += Matrix(2L, 1L) -> 3 //2,1 の組み合わせは全部同じとみなされるはず
    map += Matrix(2.0, 1.0) -> 4 //2,1 の組み合わせは全部同じとみなされるはず
    map += Matrix(2, 1, 0) -> 5 //2,1 の組み合わせは全部同じとみなされるはず
    map += Matrix(2.0, 1.0, 0) -> 6 //2,1 の組み合わせは全部同じとみなされるはず

    map += Matrix(2.1, 1.0) -> 10
    map += Matrix(2, 1.1) -> 20
    map += Matrix(2, 1, 0.1) -> 30

    assert(map.size === 4)
    assert(map(Matrix(2.0, 1.0)) === 6)
    assert(map(Matrix(2.1, 1)) === 10)
    assert(map(Matrix(2.0, 1.1)) === 20)
    assert(map(Matrix(2.0, 1L, 0.10)) === 30)
    assert(map.contains(Matrix(1, 2, 0)) === false)
  }

  @Test def negativeMatrixTest {
    //Int2D
    val mInt2D = Matrix(1, 2)
    assert(mInt2D.product(-1) === Matrix(-1, -2))
    assert(mInt2D.product(-1L) === Matrix(-1, -2))
    assert(mInt2D.product(-1.0) === Matrix(-1, -2))
    assert(mInt2D.product(-1.0f) === Matrix(-1, -2))

    assert((mInt2D.product(-1)).isInstanceOf[NegativeMatrixWithInt2D])

    //2DIntがDoubleに精度が変わる掛け算はNegativeMatrix対応しないことにする
    //assert((mInt2D.product(-1L)).isInstanceOf[NegativeMatrixWithDouble2D])
    //assert((mInt2D.product(-1.0)).isInstanceOf[NegativeMatrixWithDouble2D])
    //assert((mInt2D.product(-1.0f)).isInstanceOf[NegativeMatrixWithDouble2D])

    assert((mInt2D.product(-1).product(-1)) === mInt2D)
    assert((mInt2D.product(-1L).product(-1L)) === mInt2D)
    assert((mInt2D.product(-1.0).product(-1.0)) === mInt2D)
    assert((mInt2D.product(-1).product(-1)) eq mInt2D) //オブジェクトが使いまわされることをテスト

    assert((mInt2D.product(-1).product(-1.0).product(-1L).product(-1f)) === mInt2D)

    //
    //Int3D
    val mInt3D = Matrix(1, 2, 3)
    assert(mInt3D.product(-1) === Matrix(-1, -2, -3))
    assert(mInt3D.product(-1L) === Matrix(-1, -2, -3))
    assert(mInt3D.product(-1.0) === Matrix(-1, -2, -3))
    assert(mInt3D.product(-1.0f) === Matrix(-1, -2, -3))

    assert((mInt3D.product(-1)).isInstanceOf[NegativeMatrixWithInt])

    assert((mInt3D.product(-1L)).isInstanceOf[NegativeMatrixWithDouble])
    assert((mInt3D.product(-1.0)).isInstanceOf[NegativeMatrixWithDouble])
    assert((mInt3D.product(-1.0f)).isInstanceOf[NegativeMatrixWithDouble])

    assert((mInt3D.product(-1).product(-1)) === mInt3D)
    assert((mInt3D.product(-1L).product(-1L)) === mInt3D)
    assert((mInt3D.product(-1.0).product(-1.0)) === mInt3D)
    assert((mInt3D.product(-1).product(-1)) eq mInt3D) //2回-1を掛けるとオブジェクトが使いまわされることをテスト

    assert((mInt3D.product(-1).product(-1.0).product(-1L).product(-1f)) === mInt3D)


    //Double2D
    val mDouble2D = Matrix(1.1, 2.2)
    assert(mDouble2D.product(-1) === Matrix(-1.1, -2.2))
    assert(mDouble2D.product(-1L) === Matrix(-1.1, -2.2))
    assert(mDouble2D.product(-1.0) === Matrix(-1.1, -2.2))
    assert(mDouble2D.product(-1.0f) === Matrix(-1.1, -2.2))

    assert((mDouble2D.product(-1)).isInstanceOf[NegativeMatrixWithDouble2D])

    assert((mDouble2D.product(-1L)).isInstanceOf[NegativeMatrixWithDouble2D])
    assert((mDouble2D.product(-1.0)).isInstanceOf[NegativeMatrixWithDouble2D])
    assert((mDouble2D.product(-1.0f)).isInstanceOf[NegativeMatrixWithDouble2D])

    assert((mDouble2D.product(-1).product(-1)) === mDouble2D)
    assert((mDouble2D.product(-1L).product(-1L)) === mDouble2D)
    assert((mDouble2D.product(-1.0).product(-1.0)) === mDouble2D)
    assert((mDouble2D.product(-1).product(-1)) eq mDouble2D) //2回-1を掛けるとオブジェクトが使いまわされることをテスト

    assert((mDouble2D.product(-1).product(-1.0).product(-1L).product(-1f)) === mDouble2D)


    //Double3D
    val mDouble3D = Matrix(1.1, 2.2, 3.3)
    assert(mDouble3D.product(-1) === Matrix(-1.1, -2.2, -3.3))
    assert(mDouble3D.product(-1L) === Matrix(-1.1, -2.2, -3.3))
    assert(mDouble3D.product(-1.0) === Matrix(-1.1, -2.2, -3.3))
    assert(mDouble3D.product(-1.0f) === Matrix(-1.1, -2.2, -3.3))

    assert((mDouble3D.product(-1)).isInstanceOf[NegativeMatrixWithDouble])
    assert((mDouble3D.product(-1L)).isInstanceOf[NegativeMatrixWithDouble])
    assert((mDouble3D.product(-1.0)).isInstanceOf[NegativeMatrixWithDouble])
    assert((mDouble3D.product(-1.0f)).isInstanceOf[NegativeMatrixWithDouble])

    assert((mDouble3D.product(-1).product(-1)) === mDouble3D)
    assert((mDouble3D.product(-1L).product(-1L)) === mDouble3D)
    assert((mDouble3D.product(-1.0).product(-1.0)) === mDouble3D)
    assert((mDouble3D.product(-1).product(-1)) eq mDouble3D) //2回-1を掛けるとオブジェクトが使いまわされることをテスト

    assert((mDouble3D.product(-1).product(-1.0).product(-1L).product(-1f)) === mDouble3D)

  }
}

}