trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S]{
    def generate = f(self.generate).generate
  }
}

//val booleans = new Generator[Boolean] {
//  def generate = integers.generate > 0
//}

// Tree
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree
val integers = new Generator[Int] {
  def generate = scala.util.Random.nextInt()
}
def leafs: Generator[Leaf] = for {
  x <- integers
} yield Leaf(x)
def inners: Generator[Inner] = for {
  l <- trees
  r <- trees
} yield Inner(l, r)
def trees: Generator[Tree] = for {
  isLeaf <- integers.map(_ >= Integer.MAX_VALUE/100)
  tree <- if (isLeaf) leafs else inners
} yield tree
trees.generate
trees.generate
trees.generate

