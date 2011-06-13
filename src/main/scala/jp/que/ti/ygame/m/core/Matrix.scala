package jp.que.ti.ygame.m.core {

/**
 * x,y,z の3つの要素を扱える行列を表現する抽象trait。
 * インスタンス生成は、object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用する。
 * @author yhj
 */
sealed trait Matrix {
  /**[[scala.Int]]型のx座標 */
  def xByInt: Int

  /**[[scala.Int]]型のy座標 */
  def yByInt: Int

  /**[[scala.Int]]型のz座標 */
  def zByInt: Int

  /**x座標 */
  def x: Double

  /**y座標 */
  def y: Double

  /**z座標 */
  def z: Double

  override def equals(any: Any): Boolean = any match {
    case matrix: Matrix => x == matrix.x && y == matrix.y && z == matrix.z
    case other => false
  }

  override def hashCode: Int = 9999991 + 10007919 * xByInt + 10093 * yByInt + 2053 * zByInt

  override def toString: String = getClass.getSimpleName + "(" + x + " ," + y + " ," + z + ")"

  private final def _add(matrix: Matrix): Matrix = Matrix(x + matrix.x, y + matrix.y, z + matrix.z)

  /**
   * この行列と引数の行列の和を返却する
   * @param matrix 和を計算する対象の行列
   * @return 行列の和
   */
  def add(matrix: Matrix): Matrix = _add(matrix)

  /**
   * この行列と引数の行列の和を返却する
   * @param matrix 和を計算する対象の行列
   * @return 行列の和
   */
  final def +(matrix: Matrix): Matrix = add(matrix)

  def add(matrix: MatrixWithInt2D): Matrix = _add(matrix)

  final def +(matrix: MatrixWithInt2D): Matrix = add(matrix)

  def add(matrix: MatrixWithInt): Matrix = _add(matrix)

  final def +(matrix: MatrixWithInt): Matrix = add(matrix)

  def add(matrix: MatrixWithDouble2D): Matrix = _add(matrix)

  final def +(matrix: MatrixWithDouble2D): Matrix = add(matrix)

  //*************

  /**
   * この行列の実数倍(引数)を返却する
   * @param scalarValue 倍数
   * @return 行列の実数倍
   */
  def product(scalarValue: Long): Matrix = scalarValue match {
    case -1 => NegativeMatrixWithDouble(this)
    case other => Matrix(x * scalarValue, y * scalarValue, z * scalarValue)
  }


  /**
   * この行列の実数倍(引数)を返却する
   * @param scalarValue 倍数
   * @return 行列の実数倍
   */
  def product(scalarValue: Int): Matrix = scalarValue match {
    case -1 => NegativeMatrixWithDouble(this)
    case other => Matrix(x * scalarValue, y * scalarValue, z * scalarValue)
  }


  /**
   * この行列の実数倍(引数)を返却する
   * @param scalarValue 倍数
   * @return 行列の実数倍
   */
  def product(scalarValue: Double): Matrix = scalarValue match {
    case -1 => NegativeMatrixWithDouble(this)
    case other => Matrix(x * scalarValue, y * scalarValue, z * scalarValue)
  }

  /**
   * この行列に実数倍(引数)した行列と、この行列との和を返却する
   * @param scalarValue 倍数
   * @return 実数倍した行列とこの行列の和
   */
  def productValueAndAdd(scalarValue: Long): Matrix = Matrix(x + x * scalarValue, y + y * scalarValue, z + z * scalarValue)

  /**
   * この行列に実数倍(引数)した行列と、この行列との和を返却する
   * @param scalarValue 倍数
   * @return 実数倍した行列とこの行列の和
   */
  def productValueAndAdd(scalarValue: Int): Matrix = Matrix(x + x * scalarValue, y + y * scalarValue, z + z * scalarValue)

  /**
   * この行列に実数倍(引数)した行列と、この行列との和を返却する
   * @param scalarValue 倍数
   * @return 実数倍した行列とこの行列の和
   */
  def productValueAndAdd(scalarValue: Double): Matrix = Matrix(x + x * scalarValue, y + y * scalarValue, z + z * scalarValue)
}

/**
 * [[jp.que.ti.ygame.m.core.Matrix]]のインスタンスを生成するためのシングルトンオブジェクト
 */
object Matrix {
  /**
   * [[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド
   * @param x X座標
   * @param y Y座標
   * @param z Z座標
   */
  def apply(x: Int, y: Int, z: Int): MatrixWithInt = MatrixWithInt(x, y, z)

  /**
   * [[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド
   * @param x X座標
   * @param y Y座標
   */
  def apply(x: Int, y: Int): MatrixWithInt2D = MatrixWithInt2D(x, y)

  /**
   * [[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド
   * @param x X座標
   * @param y Y座標
   * @param z Z座標
   */
  def apply(x: Double, y: Double, z: Double): MatrixWithDouble = MatrixWithDouble(x, y, z)

  /**
   * [[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド
   * @param x X座標
   * @param y Y座標
   */
  def apply(x: Double, y: Double): MatrixWithDouble2D = MatrixWithDouble2D(x, y)

  /**すべての要素がIntの最小値である[[jp.que.ti.ygame.m.core.Matrix]]インスタンス */
  val intMin: Matrix = MatrixWithInt(Int.MinValue, Int.MinValue, Int.MinValue)
  /**すべての要素が0である[[jp.que.ti.ygame.m.core.Matrix]]インスタンス */
  val Zero: Matrix = MatrixWithInt2D(0, 0)
}

/**
 * 座標を表すクラス。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 */
trait MatrixWithDouble extends Matrix {
  def xByInt: Int = x.intValue

  def yByInt: Int = y.intValue

  def zByInt: Int = z.intValue
}

private[core] object MatrixWithDouble {
  def apply(x: Double, y: Double, z: Double): MatrixWithDouble = new MatrixWithDoubleImpl(x, y, z)
}

private[core] class MatrixWithDoubleImpl private[core](val x: Double, val y: Double, val z: Double) extends MatrixWithDouble

/**
 * 座標を表すクラス。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 */
trait MatrixWithInt extends Matrix {

  override def x = xByInt

  override def y = yByInt

  override def z = zByInt

  override def add(matrix: Matrix): Matrix = matrix.add(this)

  override def add(matrix: MatrixWithInt): MatrixWithInt =
    Matrix(xByInt + matrix.xByInt, yByInt + matrix.yByInt, zByInt + matrix.zByInt)

  override def add(matrix: MatrixWithInt2D): MatrixWithInt =
    Matrix(xByInt + matrix.xByInt, yByInt + matrix.yByInt, zByInt)

  override def product(scalarValue: Int): MatrixWithInt = scalarValue match {
    case -1 => NegativeMatrixWithInt(this)
    case other => Matrix(xByInt * scalarValue, yByInt * scalarValue, zByInt * scalarValue)
  }


  override def productValueAndAdd(scalarValue: Int): Matrix =
    Matrix(xByInt + xByInt * scalarValue, yByInt + yByInt * scalarValue, zByInt + zByInt * scalarValue)
}

private[core] object MatrixWithInt {
  def apply(x: Int, y: Int, z: Int): MatrixWithInt = new MatrixWithIntImpl(x, y, z)
}

private[core] class MatrixWithIntImpl private[core](val xByInt: Int, val yByInt: Int, val zByInt: Int) extends MatrixWithInt

/**
 * 座標を表すクラス。Z座標は0固定のクラスです。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 */
trait MatrixWithDouble2D extends Matrix {
  override def toString: String = getClass.getSimpleName + "(" + x + " ," + y + ")"

  def xByInt: Int = x.intValue

  def yByInt: Int = y.intValue

  def zByInt: Int = 0

  def z: Double = zByInt

  override def add(matrix: Matrix): Matrix = matrix.add(this)

  override def add(matrix: MatrixWithDouble2D): MatrixWithDouble2D = MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  override def add(matrix: MatrixWithInt2D): MatrixWithDouble2D = MatrixWithDouble2D(x + matrix.x, y + matrix.y)


  override def product(scalarValue: Int): MatrixWithDouble2D = scalarValue match {
    case -1 => NegativeMatrixWithDouble2D(this)
    case other => Matrix(x * scalarValue, y * scalarValue)
  }

  override def product(scalarValue: Double): MatrixWithDouble2D = scalarValue match {
    case -1 => NegativeMatrixWithDouble2D(this)
    case other => MatrixWithDouble2D(x * scalarValue, y * scalarValue)
  }

  override def product(scalarValue: Long): MatrixWithDouble2D = scalarValue match {
    case -1 => NegativeMatrixWithDouble2D(this)
    case other => MatrixWithDouble2D(x * scalarValue, y * scalarValue)
  }

  override def productValueAndAdd(scalarValue: Double): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

  override def productValueAndAdd(scalarValue: Long): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

  override def productValueAndAdd(scalarValue: Int): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)
}

private[core] object MatrixWithDouble2D {
  def apply(x: Double, y: Double): MatrixWithDouble2D = new MatrixWithDouble2DImpl(x, y)
}

private[core] class MatrixWithDouble2DImpl private[core](val x: Double, val y: Double) extends MatrixWithDouble2D

/**
 * 座標を表すクラス。Z座標は0固定のクラスです。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 */
trait MatrixWithInt2D extends Matrix {
  override def toString: String = getClass.getSimpleName + "(" + x + " ," + y + ")"

  def zByInt: Int = 0

  override def x = xByInt

  override def y = yByInt

  override def z = zByInt

  override def add(matrix: Matrix): Matrix = matrix.add(this)

  override def add(matrix: MatrixWithInt2D): MatrixWithInt2D =
    MatrixWithInt2D(xByInt + matrix.xByInt, yByInt + matrix.yByInt)

  override def add(matrix: MatrixWithInt): MatrixWithInt =
    MatrixWithInt(xByInt + matrix.xByInt, yByInt + matrix.yByInt, matrix.zByInt)

  override def add(matrix: MatrixWithDouble2D): MatrixWithDouble2D =
    MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  override def product(scalarValue: Int): MatrixWithInt2D = scalarValue match {
    case -1 => NegativeMatrixWithInt2D(this)
    case other => MatrixWithInt2D(xByInt * scalarValue, yByInt * scalarValue)
  }

  override def product(scalarValue: Double): MatrixWithDouble2D = MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def product(scalarValue: Long): MatrixWithDouble2D = MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def productValueAndAdd(scalarValue: Double): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

  override def productValueAndAdd(scalarValue: Long): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

  override def productValueAndAdd(scalarValue: Int): MatrixWithInt2D =
    MatrixWithInt2D(xByInt + xByInt * scalarValue, yByInt + yByInt * scalarValue)
}

private[core] object MatrixWithInt2D {
  def apply(x: Int, y: Int): MatrixWithInt2D = new MatrixWithInt2DImpl(x, y)
}

private[core] class MatrixWithInt2DImpl private[core](val xByInt: Int, val yByInt: Int) extends MatrixWithInt2D

trait NegativeMatrix extends Matrix {
  type t <: Matrix
  val matrix: t

  /**[[scala.Int]]型のx座標 */
  override def xByInt: Int = matrix.xByInt * -1

  /**[[scala.Int]]型のy座標 */
  override def yByInt: Int = matrix.yByInt * -1

  /**[[scala.Int]]型のz座標 */
  override def zByInt: Int = matrix.zByInt * -1

  /**x座標 */
  override def x: Double = matrix.x * -1

  /**y座標 */
  override def y: Double = matrix.y * -1

  /**z座標 */
  override def z: Double = matrix.z * -1
}

final private[core] case class
NegativeMatrixWithDouble(override val matrix: Matrix
                          ) extends MatrixWithDouble with NegativeMatrix {
  type t = Matrix

  override def product(scalarValue: Long): Matrix = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }

  override def product(scalarValue: Int): Matrix = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }

  override def product(scalarValue: Double): Matrix = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }

}

final private[core] case class
NegativeMatrixWithInt(override val matrix: MatrixWithInt
                       ) extends MatrixWithInt with NegativeMatrix {
  type t = MatrixWithInt

  override def product(scalarValue: Int): MatrixWithInt = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }

}

final private[core] case class
NegativeMatrixWithDouble2D(override val matrix: MatrixWithDouble2D
                            ) extends MatrixWithDouble2D with NegativeMatrix {
  type t = MatrixWithDouble2D

  override def product(scalarValue: Int): MatrixWithDouble2D = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }

  override def product(scalarValue: Long): MatrixWithDouble2D = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }

  override def product(scalarValue: Double): MatrixWithDouble2D = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }
}

final private[core] case class
NegativeMatrixWithInt2D(override val matrix: MatrixWithInt2D
                         ) extends MatrixWithInt2D with NegativeMatrix {
  type t = MatrixWithInt2D

  override def product(scalarValue: Int): MatrixWithInt2D = scalarValue match {
    case -1 => matrix
    case other => super.product(scalarValue)
  }
}

}