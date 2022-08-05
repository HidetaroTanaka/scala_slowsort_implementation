import scala.math._
import scala.util.Random

object Main extends App {
  def slowsort[T: Ordering](l: List[T], f: (T, T) => Boolean): List[T] = {
    if(l.length <= 1) l
    else {
      val sorted_first_half = slowsort(l.slice(0,l.length/2), f)
      val sorted_second_half = slowsort(l.slice(l.length/2,l.length), f)
      if(f(sorted_first_half.head, sorted_second_half.head)) {
        sorted_first_half.head :: slowsort(sorted_first_half.tail ::: sorted_second_half, f)
      } else {
        sorted_second_half.head :: slowsort(sorted_first_half ::: sorted_second_half.tail, f)
      }
    }
  }

  Random.setSeed(0)
  val array_before_sort = Random.shuffle((0 until 32).toList)
  println("ソート前のリスト:")
  println(array_before_sort.mkString("(",", ",")"))
  println("昇順ソート:")
  println(slowsort[Int](array_before_sort, _ < _).mkString("(",", ",")"))
  println("降順ソート:")
  println(slowsort[Int](array_before_sort, _ > _).mkString("(",", ",")"))
}
