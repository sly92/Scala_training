object Main {

  def main(args: Array[String]): Unit = {
    if(args.length<5)
      println("Attention ! Il faut mettre 5 parametres")
    else
      {
        val c1 = ComplexNumber(args(1).toInt,args(2).toInt)
        val c2 = ComplexNumber(args(3).toInt,args(4).toInt)
        val res = args(0) match {
          case "+" => c1.+(c2).toString
          case "-" => c1.-(c2).toString
          case "*" => c1.*(c2).toString
          case _ => "L'operateur n'est pas connu"
        }
        println(res)
      }

    
  }

  case class ComplexNumber(re:Int, im:Int){
    def +(c: ComplexNumber): ComplexNumber = ComplexNumber(re + c.re, im + c.im)
    def -(c: ComplexNumber): ComplexNumber = ComplexNumber(re - c.re, im - c.im)
    def *(c: ComplexNumber): ComplexNumber = ComplexNumber(re * c.re - im*c.im, re*c.im - c.re*im)
    override def toString: String = s"$re + $im"+"i"
  }


}
