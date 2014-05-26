object Funtimes {
  def myFunMethod(numberOne: String) = 3
  def myFunMethod2(numberOne: String, numberTwo: Int) = 3
  def whatever() = {
    import unfiltered.porthole.Porthole

    Porthole(Funtimes.myFunMethod _) ::
    Porthole(Funtimes.myFunMethod2 _) ::
    Porthole({s:String => java.lang.System.out.println(s) }) :: Nil
  }
}
