import scala.math.Ordered.orderingToOrdered
import scala.util.Random

object Main extends App {
  def slowsort[T: Ordering](l: List[T]): List[T] = {
    if(l.length <= 1) l
    else {
      val sorted_first_half = slowsort(l.slice(0,l.length/2))
      val sorted_second_half = slowsort(l.slice(l.length/2, l.length))
      if(sorted_first_half.head < sorted_second_half.head) {
        sorted_first_half.head :: slowsort(sorted_first_half.tail ::: sorted_second_half)
      } else {
        sorted_second_half.head :: slowsort(sorted_first_half ::: sorted_second_half.tail)
      }
    }
  }
  Random.setSeed(0)
  val array_before_sort = Random.shuffle((0 until 32).toList)
  println("Array before sort:")
  println(array_before_sort.mkString("(",", ",")"))
  println("Array after sort:")
  println(slowsort(array_before_sort).mkString("(",", ",")"))
}
