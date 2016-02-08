import fpinscala.monoids._

Monoid.foldRight(List(1, 2, 3, 4))(3.0)((x, y) => y + x)

