package temp {

case class Foo(val foofoo: String) {
  def this() = this ("no param")

}

object Foo

object FooUser {
  def main(args: Array[String]) = {
    var f1: Foo = Foo("case f1")
    var f2: Foo = new Foo()

    //var f3: Foo = Foo() // コンパイルエラー
    //error: not enough arguments for method apply: (foofoo: String)temp.Foo in object Foo.
    //Unspecified value parameter foofoo.

    //var f4: Foo = Foo // コンパイルエラー
    //error: type mismatch;
    //found   : temp.Foo.type (with underlying type object temp.Foo)
    //required: temp.Foo
  }
}

}