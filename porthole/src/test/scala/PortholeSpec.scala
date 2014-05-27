package unfiltered.porthole.test

object Funtimes {
  def myFunMethod(numberOne: String) = 3
  def myFunMethod2(numberOne: String, numberTwo: Int) = 3
}
object Whatever {
  def whatever() = {
    import unfiltered.porthole.Porthole
    import unfiltered.java.JavaTest
    import unfiltered.porthole.testpackage.Test.test

    Porthole(Funtimes.myFunMethod _) ::
    Porthole(Funtimes.myFunMethod2 _) ::
    Porthole(test _) ::
    Porthole(JavaTest.test _) :: Nil
  }
}
