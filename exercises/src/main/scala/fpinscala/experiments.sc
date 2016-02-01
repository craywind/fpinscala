import fpinscala.datastructures._

val y = List.tail(List(1, 2, 3))
List.tail(List.tail(y))
//val a = List.tail(z)
List.setHead(y, 6)
List.drop(y, 2)
//val c = List.drop(y, 3)
val d = List.dropWhile(List(1, 2, 3, 4, 5, 6, 0), (x: Int) => x < 4)
List.init(y)
List.length(d)
List.length2(d)
List.sum3(d)
List.product3(List(4.0, 3, 15))
List.reverse(d)
List.reverse2(d)
List.append2(d, y)
val l = List.flatten(List(List(1, 2, 3), List(4, 5), List(6)))
List.flatten2(List(List(1, 2, 3), List(4, 5), List(6)))
List.add1(l)
List.doubleToString(List(4.0, 3, 15))
List.map(l)((x) => x + 1)
List.map2(List(4.0, 3, 15))((x) => x.toString)
List.filter(l)((x) => x < 4)
List.filter2(l)((x) => x % 2 == 0)
List.flatMap(l)((x) => Cons(x, Cons(x, Nil)))
List.flatMap2(l)((x) => Cons(x, Cons(x, Nil)))
List.filter3(l)((x) => x % 2 == 0)
List.zipAdd(List(1, 2, 3), List(4, 5, 6, 7))
List.zipWith(List(4, 5, 6), List(0, 1, 2))(_ - _)
List.hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4))
List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 4))
val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
Tree.size(t)
Tree.maximum(t)
Tree.depth(t)
Tree.depth2(t)
Tree.map(t)(_ * 2)
Tree.size2(t)
Tree.maximum2(t)
Tree.depth3(t)
Tree.map2(t)(_ * 2)

















