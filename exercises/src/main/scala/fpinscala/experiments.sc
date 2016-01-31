import fpinscala.datastructures._

val y = List.tail(List(1, 2, 3))
val z = List.tail(List.tail(y))
//val a = List.tail(z)
val b = List.setHead(y, 6)
val a = List.drop(y, 2)
//val c = List.drop(y, 3)
val d = List.dropWhile(List(1, 2, 3, 4, 5, 6, 0), (x: Int) => x < 4)
val c = List.init(y)
val e = List.length(d)
val f = List.length2(d)
val g = List.sum3(d)
val h = List.product3(List(4.0, 3, 15))
val i = List.reverse(d)
val j = List.reverse2(d)
val k = List.append2(d, y)
val l = List.flatten(List(List(1, 2, 3), List(4, 5), List(6)))
val m = List.flatten2(List(List(1, 2, 3), List(4, 5), List(6)))
val n = List.add1(l)
val o = List.doubleToString(List(4.0, 3, 15))
val p = List.map(l)((x) => x + 1)
val q = List.map2(List(4.0, 3, 15))((x) => x.toString)
val r = List.filter(l)((x) => x < 4)
val s = List.filter2(l)((x) => x % 2 == 0)










