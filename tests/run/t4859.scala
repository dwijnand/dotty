object O {
  case class N()
  object P
}

// We assume module initialisation to be pure, running this test under
// -optimise yields different results.
object Outer {
  println("Outer")
  object Inner {
    println("Inner")
    def i: Unit = {
      println("Inner.i")
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    Outer.Inner.i // we still don't initialize Outer here (but should we?)

    {println("About to reference Inner.i"); Outer}.Inner.i // Outer will be initialized.

    {println("About to reference O.N"        ); O}.N

    {println("About to reference O.N"        ); O}.N

    {println("About to reference O.N.apply()"); O}.N.apply()
  }
}

