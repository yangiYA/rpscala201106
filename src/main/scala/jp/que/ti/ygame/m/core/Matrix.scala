package jp.que.ti.ygame.m.core {

/**
 * x,y,z の3つの要素を扱える行列を表現する抽象trait。
 * インスタンス生成は、object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用する。
 * @author yhj
 */
sealed abstract class Matrix {
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
  def product(scalarValue: Long): Matrix = Matrix(x * scalarValue, y * scalarValue, z * scalarValue)

  /**
   * この行列の実数倍(引数)を返却する
   * @param scalarValue 倍数
   * @return 行列の実数倍
   */
  def product(scalarValue: Int): Matrix = Matrix(x * scalarValue, y * scalarValue, z * scalarValue)

  /**
   * この行列の実数倍(引数)を返却する
   * @param scalarValue 倍数
   * @return 行列の実数倍
   */
  def product(scalarValue: Double): Matrix = Matrix(x * scalarValue, y * scalarValue, z * scalarValue)

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
  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Int, y: Int, z: Int): MatrixWithInt = new MatrixWithInt(x, y, z)

  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Int, y: Int): MatrixWithInt2D = new MatrixWithInt2D(x, y)

  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Double, y: Double, z: Double): MatrixWithDouble = new MatrixWithDouble(x, y, z)

  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Double, y: Double): MatrixWithDouble2D = new MatrixWithDouble2D(x, y)

  /**すべての要素がIntの最小値である[[jp.que.ti.ygame.m.core.Matrix]]インスタンス */
  val intMin: Matrix = new MatrixWithInt(Int.MinValue, Int.MinValue, Int.MinValue)
  /**すべての要素が0である[[jp.que.ti.ygame.m.core.Matrix]]インスタンス */
  val Zero: Matrix = new MatrixWithInt2D(0, 0)
}

/**
 * 座標を表すクラス。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 *
 * @param xpos X座標
 * @param ypos Y座標
 * @param zpos Z座標
 */
class MatrixWithDouble private[core](val x: Double, val y: Double, val z: Double) extends Matrix {
  def xByInt: Int = x.intValue

  def yByInt: Int = y.intValue

  def zByInt: Int = z.intValue
}

/**
 * 座標を表すクラス。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 *
 * @param xpos X座標
 * @param ypos Y座標
 * @param zpos Z座標
 */
class MatrixWithInt private[core](val xByInt: Int, val yByInt: Int, val zByInt: Int) extends Matrix {

  override def x = xByInt

  override def y = yByInt

  override def z = zByInt

  override def add(matrix: Matrix): Matrix = matrix.add(this)

  override def add(matrix: MatrixWithInt): MatrixWithInt =
    Matrix(xByInt + matrix.xByInt, yByInt + matrix.yByInt, zByInt + matrix.zByInt)

  override def add(matrix: MatrixWithInt2D): MatrixWithInt =
    Matrix(xByInt + matrix.xByInt, yByInt + matrix.yByInt, zByInt)

  override def product(scalarValue: Int): MatrixWithInt =
    Matrix(xByInt * scalarValue, yByInt * scalarValue, zByInt * scalarValue)

  override def productValueAndAdd(scalarValue: Int): Matrix =
    Matrix(xByInt + xByInt * scalarValue, yByInt + yByInt * scalarValue, zByInt + zByInt * scalarValue)
}

/**
 * Z座標は0固定の座標を表すtrait
 * @author yhj
 */
private[core] trait Matrix2D extends Matrix {
  def zByInt: Int = 0

  override def toString: String = getClass.getSimpleName + "(" + x + " ," + y + ")"

  override def product(scalarValue: Double): MatrixWithDouble2D = new MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def product(scalarValue: Long): MatrixWithDouble2D = new MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def productValueAndAdd(scalarValue: Double): MatrixWithDouble2D =
    new MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

  override def productValueAndAdd(scalarValue: Long): MatrixWithDouble2D =
    new MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

}

/**
 * 座標を表すクラス。Z座標は0固定のクラスです。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 *
 * @param xpos X座標
 * @param ypos Y座標
 */
class MatrixWithDouble2D private[core](val x: Double, val y: Double) extends Matrix2D {
  def xByInt: Int = x.intValue

  def yByInt: Int = y.intValue

  def z: Double = zByInt

  override def add(matrix: Matrix): Matrix = matrix.add(this)

  override def add(matrix: MatrixWithDouble2D): MatrixWithDouble2D = new MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  override def add(matrix: MatrixWithInt2D): MatrixWithDouble2D = new MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  override def product(scalarValue: Int): MatrixWithDouble2D = new MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def productValueAndAdd(scalarValue: Int): MatrixWithDouble2D =
    new MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)
}

/**
 * 座標を表すクラス。Z座標は0固定のクラスです。当クラスは imutable です
 * <br/>
 * このクラスのコンストラクタはprivateです。インスタンス生成は、
 * object [[jp.que.ti.ygame.m.core.Matrix]]の apply メソッドを使用してください。
 * @author yhj
 *
 * @param xpos X座標
 * @param ypos Y座標
 */
class MatrixWithInt2D private[core](val xByInt: Int, val yByInt: Int) extends Matrix2D {
  override def x = xByInt

  override def y = yByInt

  override def z = zByInt

  override def add(matrix: Matrix): Matrix = matrix.add(this)

  override def add(matrix: MatrixWithInt2D): MatrixWithInt2D =
    new MatrixWithInt2D(xByInt + matrix.xByInt, yByInt + matrix.yByInt)

  override def add(matrix: MatrixWithInt): MatrixWithInt =
    new MatrixWithInt(xByInt + matrix.xByInt, yByInt + matrix.yByInt, matrix.zByInt)

  override def add(matrix: MatrixWithDouble2D): MatrixWithDouble2D =
    new MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  override def product(scalarValue: Int): MatrixWithInt2D =
    new MatrixWithInt2D(xByInt * scalarValue, yByInt * scalarValue)

  override def productValueAndAdd(scalarValue: Int): MatrixWithInt2D =
    new MatrixWithInt2D(xByInt + xByInt * scalarValue, yByInt + yByInt * scalarValue)
}

abstract trait NegativeMatrix extends Matrix {
  type t <: Matrix
  val matrix: t

  /**[[scala.Int]]型のx座標 */
  def xByInt: Int = matrix.xByInt * -1

  /**[[scala.Int]]型のy座標 */
  def yByInt: Int = matrix.yByInt * -1

  /**[[scala.Int]]型のz座標 */
  def zByInt: Int = matrix.zByInt * -1

  /**x座標 */
  def x: Double = matrix.x * -1

  /**y座標 */
  def y: Double = matrix.y * -1

  /**z座標 */
  def z: Double = matrix.z * -1

}

final private[core] case class
NegativeMatrixWithInt2D(
                         override val matrix: MatrixWithInt2D
                         ) extends NegativeMatrix with Matrix2D {
  type t = MatrixWithInt2D

  override def zByInt = 0
}

}