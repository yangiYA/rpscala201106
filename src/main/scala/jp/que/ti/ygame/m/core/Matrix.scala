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

  /**
   * この行列と引数の行列の和を返却する
   * @param matrix 和を計算する対象の行列
   * @return 行列の和
   */
  def add(matrix: Matrix): Matrix = Matrix(x + matrix.x, y + matrix.y, z + matrix.z)

  /**
   * この行列と引数の行列の和を返却する
   * @param matrix 和を計算する対象の行列
   * @return 行列の和
   */
  def +(matrix: Matrix): Matrix = add(matrix)

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
  def apply(x: Int, y: Int, z: Int): MatrixWithInt = MatrixWithInt(x, y, z)

  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Int, y: Int): MatrixWithInt2D = MatrixWithInt2D(x, y)

  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Double, y: Double, z: Double): MatrixWithDouble = MatrixWithDouble(x, y, z)

  /**[[jp.que.ti.ygame.m.core.Matrix]]インスタンス生成メソッド */
  def apply(x: Double, y: Double): MatrixWithDouble2D = MatrixWithDouble2D(x, y)

  /**すべての要素がIntの最小値である[[jp.que.ti.ygame.m.core.Matrix]]インスタンス */
  val intMin: Matrix = MatrixWithInt(Int.MinValue, Int.MinValue, Int.MinValue)
  /**すべての要素が0である[[jp.que.ti.ygame.m.core.Matrix]]インスタンス */
  val Zero: Matrix = MatrixWithInt(0, 0, 0)
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
final case class MatrixWithDouble private[core](x: Double, y: Double, z: Double) extends Matrix {
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
final case class MatrixWithInt private[core](xByInt: Int, yByInt: Int, zByInt: Int) extends MatrixWithIntBase {
  override def add(matrix: Matrix): Matrix = matrix match {
    case mInt: MatrixWithInt => super.add(mInt)
    case mInt2D: MatrixWithInt2D => super.add(mInt2D)
    case mtx => super.add(mtx)
  }

  override def product(scalarValue: Int): MatrixWithInt = MatrixWithInt(xByInt * scalarValue, yByInt * scalarValue, zByInt * scalarValue)

  override def productValueAndAdd(scalarValue: Int): Matrix = Matrix(xByInt + xByInt * scalarValue, yByInt + yByInt * scalarValue, zByInt + zByInt * scalarValue)
}

/**
 * Z座標は0固定の座標を表すtrait
 * @author yhj
 */
private[core] abstract trait Matrix2D extends Matrix {
  def zByInt: Int = 0

  override def product(scalarValue: Double): MatrixWithDouble2D = MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def product(scalarValue: Long): MatrixWithDouble2D = MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def productValueAndAdd(scalarValue: Double): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

  override def productValueAndAdd(scalarValue: Long): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)

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
final case class MatrixWithDouble2D private[core](x: Double, y: Double) extends Matrix with Matrix2D {
  def xByInt: Int = x.intValue

  def yByInt: Int = y.intValue

  def z: Double = zByInt

  def add(matrix: MatrixWithDouble2D): MatrixWithDouble2D = MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  def +(matrix: MatrixWithDouble2D): MatrixWithDouble2D = add(matrix)

  def add(matrix: MatrixWithInt2D): MatrixWithDouble2D = MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  def +(matrix: MatrixWithInt2D): MatrixWithDouble2D = add(matrix)

  override def add(matrix: Matrix): Matrix = matrix match {
    case mInt2D: MatrixWithInt2D => MatrixWithDouble2D(x + mInt2D.x, y + mInt2D.y)
    case mtx => super.add(mtx)
  }

  override def product(scalarValue: Int): MatrixWithDouble2D = MatrixWithDouble2D(x * scalarValue, y * scalarValue)

  override def productValueAndAdd(scalarValue: Int): MatrixWithDouble2D =
    MatrixWithDouble2D(x + x * scalarValue, y + y * scalarValue)
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
final case class MatrixWithInt2D private[core](xByInt: Int, yByInt: Int) extends MatrixWithIntBase with Matrix2D {
  def add(matrix: MatrixWithInt2D): MatrixWithInt2D = MatrixWithInt2D(xByInt + matrix.xByInt, yByInt + matrix.yByInt)

  def +(matrix: MatrixWithInt2D): MatrixWithInt2D = add(matrix)

  override def add(matrix: Matrix): Matrix = matrix match {
    case mInt2D: MatrixWithInt2D => MatrixWithInt2D(xByInt + mInt2D.xByInt, yByInt + mInt2D.yByInt)
    case mDouble2D: MatrixWithDouble2D => MatrixWithDouble2D(x + mDouble2D.x, y + mDouble2D.y)
    case mInt: MatrixWithInt => super.add(mInt)
    case mtx => super.add(mtx)
  }

  def add(matrix: MatrixWithDouble2D): MatrixWithDouble2D = MatrixWithDouble2D(x + matrix.x, y + matrix.y)

  def +(matrix: MatrixWithDouble2D): MatrixWithDouble2D = add(matrix)

  override def add(matrix: MatrixWithInt): MatrixWithInt = MatrixWithInt(xByInt + matrix.xByInt, yByInt + matrix.yByInt, matrix.zByInt)

  override def +(matrix: MatrixWithInt): MatrixWithInt = add(matrix)

  override def add(matrix: MatrixWithIntBase): MatrixWithIntBase = matrix match {
    case pointWithInt2D: MatrixWithInt2D => add(pointWithInt2D)
    case pointWithInt: MatrixWithInt => add(pointWithInt)
    case other => super.add(other)
  }

  override def product(scalarValue: Int): MatrixWithInt2D = MatrixWithInt2D(xByInt * scalarValue, yByInt * scalarValue)

  override def productValueAndAdd(scalarValue: Int): MatrixWithInt2D =
    MatrixWithInt2D(xByInt + xByInt * scalarValue, yByInt + yByInt * scalarValue)
}

/**
 * 座標の要素をInt型で管理するを座標クラス。当クラスはマーキングが主目的のクラスです
 * @author yhj
 *
 * @param xpos X座標
 * @param ypos Y座標
 * @param zpos Z座標
 */
private[core] abstract trait MatrixWithIntBase extends Matrix {
  override def x = xByInt

  override def y = yByInt

  override def z = zByInt

  def add(point: MatrixWithInt): MatrixWithInt = MatrixWithInt(xByInt + point.xByInt, yByInt + point.yByInt, zByInt + point.zByInt)

  def +(matrix: MatrixWithInt): MatrixWithInt = add(matrix)

  def add(point: MatrixWithIntBase): MatrixWithIntBase = MatrixWithInt(xByInt + point.xByInt, yByInt + point.yByInt, zByInt + point.zByInt)

  def +(matrix: MatrixWithIntBase): MatrixWithIntBase = add(matrix)

}

}