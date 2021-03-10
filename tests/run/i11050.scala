import scala.compiletime.*
import scala.deriving.*

sealed trait TreeValue

sealed trait SubLevel extends TreeValue

case class Leaf1(value: String) extends TreeValue
case class Leaf2(value: Int)    extends SubLevel
case class Leaf3(value: Char)   extends TreeValue

object Test:
  val m                 = summon[Mirror.SumOf[TreeValue]]
  given Show[TreeValue] = Show.derived[TreeValue]

  def main(args: Array[String]) =
    val leaf1 = Leaf1("1")
    val leaf2 = Leaf2(2)
    val leaf3 = Leaf3('3')

    assertEq(List(leaf1, leaf2, leaf3).map(m.ordinal), List(0, 1, 2))
    assertShow(leaf1, "Leaf1(value = \"1\")")
    assertShow(leaf2, "Leaf2(value = 2)")
    assertShow(leaf3, "Leaf3(value = '3')")
  end main

  def assertEq[A](obt: A, exp: A)          = assert(obt == exp, s"Expected $obt == $exp")
  def assertShow[A: Show](x: A, s: String) = assertEq(Show.show(x), s)
end Test

trait Show[-T]:
  def show(x: T): String

object Show:
  given Show[Int]    with { def show(x: Int)    = s"$x"     }
  given Show[Char]   with { def show(x: Char)   = s"'$x'"   }
  given Show[String] with { def show(x: String) = s"$"$x$"" }

  inline def show[T](x: T): String = summonInline[Show[T]].show(x)

  transparent inline def derived[T](implicit ev: Mirror.Of[T]): Show[T] = new {
    def show(x: T): String = inline ev match {
      case m: Mirror.ProductOf[T] => showProduct(x.asInstanceOf[Product], m)
      case m: Mirror.SumOf[T]     => showCases[m.MirroredElemTypes](0)(x, m.ordinal(x))
    }
  }

  inline def showProduct[T](x: Product, m: Mirror.ProductOf[T]): String =
    constValue[m.MirroredLabel] + showElems[m.MirroredElemTypes, m.MirroredElemLabels](0, Nil)(x)

  inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int, elems: List[String])(x: Product): String =
    inline (erasedValue[Labels], erasedValue[Elems]) match {
      case _: (label *: labels, elem *: elems) =>
        val value = show(x.productElement(n).asInstanceOf[elem])
        showElems[elems, labels](n + 1, s"${constValue[label]} = $value" :: elems)(x)
      case _: (EmptyTuple, EmptyTuple)         =>
        if elems.isEmpty then "" else elems.mkString(s"(", ", ", ")")
    }

  transparent inline def showCases[Alts <: Tuple](n: Int)(x: Any, ord: Int): String =
    inline erasedValue[Alts] match {
      case _: (alt *: alts) =>
        if (ord == n) summonFrom {
          case m: Mirror.Of[`alt`] => derived[alt](using m).show(x.asInstanceOf[alt])
        } else showCases[alts](n + 1)(x, ord)
      case _: EmptyTuple => throw new MatchError(x)
    }
end Show
