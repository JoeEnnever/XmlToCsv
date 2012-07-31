package learning

/**
 *
 * <br>Date: 7/3/12
 * @author Joe Ennever
 */

class Person(val firstName: String, val lastName: String) extends Ordered[Person] {
  require(firstName != null)
  require(lastName != null)

  override def equals(o: Any) = {
    if (!(o.isInstanceOf[Person])) false
    else {
      val that = o.asInstanceOf[Person]
      this.firstName.equalsIgnoreCase(that.firstName) && this.lastName.equalsIgnoreCase(that.lastName)
    }
  }

  def compare(that: Person) = {
    val lNComp = lastName.compareToIgnoreCase(that.lastName)
    if (lNComp != 0)
      lNComp
    else
      firstName.compareToIgnoreCase(that.firstName)
  }
}

case class :::[T](head: T, tail: List[T])

