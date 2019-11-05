object Other {

      case class User(name: String, address: String)

      def addressOf(users: List[User]): List[String] = users.map(user => user.address)

      case class Employee(name: String, salary: Double)

      def salarySum(employees: List[Employee]): Double = {employees.map(_.salary).sum}

      def add1(o1: Option[Int], o2: Option[Int]): Option[Int] = {
        if(o1.isDefined && o2.isDefined)
          Some(o1.get + o2.get)
        else
          None
      }

      def add2(o1: Option[Int], o2: Option[Int]): Option[Int] = {
        o1.map(_+o2.get)
      }

      def add3(o1: Option[Int], o2: Option[Int]): Option[Int] = {
        for {
          x1 <- o1
          x2 <- o2
        } yield x1 + x2
      }
      def average(values: Iterator[Double]): Option[Double] = {
        val (s,l) = values.foldLeft((0D, 0D)) {
          (acc: (Double, Double), e: Double) => {
            val sum = acc._1
            val length = acc._2
            (sum + e, length + 1)
          }
        }
        if(l==0) Some(s/l)
        else None
      }


      case class Book (title: String , authors: List [ String ])
      val books: List [ Book ] = List (
        Book ( "Structure and Interpretation of Computer Programs" ,
          List ( "Abelson, Harold" , "Sussman, Gerald J." )),
        Book ( "Principles of Compiler Design" ,
          List ( "Aho, Alfred" , "Ullman, Jeffrey" )),
        Book ( "Programming in Modula‑2" ,
          List ( "Wirth, Niklaus" )),
        Book ( "Introduction to Functional Programming" ,
          List ( "Bird, Richard","Joy, Bill" )),
        Book ( "The Java Language Specification" ,
          List ( "Gosling, James" , "Joy, Bill" , "Steele, Guy" , "Bracha Gilad" )))

      def findBooksWithAuthor(books: List[Book], author: String): List[String] = {
        for {
          book <- books if book.authors.contains(author)
        } yield book.title
      }

      def getAllBooks(name: String): List[String] = {
        for {
          book <- books if book.title.contains(name)
        } yield book.title
      }

      def getAll2BooksWriter: List[String] = {
        for {
          book1 <- books
          book2 <- books if book2!=book1
          a1 <- book1.authors
          a2 <- book2.authors if a2 == a1
        } yield a1
      }

  def deduplicates[A](list: List[A]): List[A] = {
    list.foldLeft(List[A]())(
      (acc, e) => if(acc.contains(e)){
        acc
      }
      else {
        acc :+ e
      }
    )
  }
}




