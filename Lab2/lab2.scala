/****************************************/
/* Lab 2/Tut 4 : Higher-Order Functions */
/****************************************/

/* As there is no tutorial on 12Sept, this exercise will be just a homework */
/* This lab assignment is to be submitted by 28th Sept 2016 6pm */
/* In case you have problems, please email your lecturer or tutor for consultation */

object Lab2 extends App {
  println("CS2104 Lab2")
 /*
  Q1: Consider the following implementation of reverse
      using List#foldLeft. It contains a bug which causes its
      result to be wrong. Can you fix it so that you get the following
      correct outcomes?

# reverse (List(1,2,3))
- : List[Int] = List(3, 2, 1)
# reverse (List(1,2))
- : List[Int] = List(2, 1)
# reverse (List())
- : List[Nothing] = List()

*/
/* fix a bug in the code below */
  def reverse [X](xs:List[X]) : List[X] =
      xs.foldLeft (List[X]()) ((acc,x)=>(x::xs))

/*
  Q2 : List Operations

  Using only List#map, List#filter and List#flatten,
  write code for the following list operations. You
  may use let constructs to name intermediate computations.

*/

/*
   (a) cross-product of two lists that returns an element
       from each list as a pair (a,b).
       That is [(a,b) | a <- xs, b <- ys]
   Example: product (List(1,2,3)) (List('a','b'))
   ==> List[(Int, Char)] = List((1,a), (1,b), (2,a), (2,b), (3,a), (3,b))
*/

  def product [A,B](xs:List[A]) (ys:List[B]) =
      throw new Exception ("TBI")


/*
   (b) cross-product of two lists that returns an integer
       from each list as a pair (a,b) such that a<=b.
       That is [(a,b) | a <- xs, b <- ys, a<=b]
   Example : product2 (List(1,2,3)) (List(2,7))
   ==>
- : List[Int,Int] = List((1, 2), (1, 7), (2, 2), (2, 7), (3, 7))
*/

  def product2(xs:List[Int]) (ys:List[Int]) =
      throw new Exception ("TBI")

/*
   (c) The divisor from a product of two lists, but only
       if the second element is non-zero.
       That is [a/b | b<-ys, b!=0, a <- xs]
   Example : divisor_prod (List(5,9,4)) (List(2,0,3))
   ==>
   - :  List[Int] = List(2, 4, 2, 1, 3, 1)
*/

  def divisor_prod(xs:List[Int]) (ys:List[Int])  =
    throw new Exception ("TBI")

/*
  Q3 : Write a function that would count the number of
       positive, negative and zero elements in a list of numbers.

      (i) Use only the List#foldRight operation
      (ii) Use only List#filter and List#length

   Example:
     count_nums1 (List(1,-4,5,8,0,-9))
     => (Int, Int ,Int) = (3, 2, 1)

*/

/* to return number of positive, negative and zero numbers */
  def count_nums1 (ys:List[Int]) : (Int, Int, Int) =
    throw new Exception("TBI  using List#foldRight")

  def count_nums2 (ys:List[Int]) : (Int, Int, Int) =
    throw new Exception("TBI  using List#filter and List#length")


/*
  Q4 : Higher-Order functions for Trees

	During the last tutorial, we implemented two higher-order
        functions for a simple binary trees as follows:

  Let us practise more examples on them.

*/
  abstract class Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  val t1 = new Node (3,new Leaf(1), new Leaf(2));;
  val t2 = new Node (4,t1,new Leaf (6));;
  val t3 = new Node (5,t2,new Leaf (3));;

/* map for tree */
  def map_tree [A,B] (f:A=>B) (tree: Tree[A]) : Tree[B] =
  	tree match {
  		case Leaf(value) => Leaf(f (value))
  		case Node(value , l, r) => Node(f (value), map_tree (f) (l), map_tree (f) (r))

  	}

   def fold_tree[A,B](f1: A => B) (f2: (A,B,B) => B) (t: Tree[A]) : B = t match {
      case Leaf(value) => f1(value)
      case Node(value , l, r) => f2 (value, fold_tree (f1) (f2) (l), fold_tree (f1) (f2) (r) )
    }

/* Use these higher-order functions to write the following codes */

/* (a) a function that would add n to every element of a tree

Example:
scala> t1
res : Node[Int] = Node(3,Leaf(1),Leaf(2))
scala> add_n(t1,3)
res : Node[Int] = Node(6,Leaf(4),Leaf(5))
*/

  def add_n (t:Tree[Int], n:Int) : Tree[Int] =
    throw new Exception("TBI")

/* (b) a function that would return the rightmost element of a tree

# t2
- : Node[Int] = Node(4,Node(3,Leaf(1),Leaf(2)),Leaf(6))
# right_most (t2)
- : Int = 6
# t3;;
- : Node[Int] = Node(5,Node(4,Node(3,Leaf(1),Leaf(2)),Leaf(6)),Leaf(3))
# right_most (t3)
- : Int = 3
*/

  def right_most [A](t:Tree[A]) : A =
    throw new Exception("TBI")

/* (c) a function that would return the mirror of a tree
        where left and right subtrees are recursively flipped

# t2
- : Node[Int] = Node(4,Node(3,Leaf(1),Leaf(2)),Leaf(6))
# mirror_tree (t2)
- : Node[Int] = Node(4,Leaf(6),Node(3,Leaf(2),Leaf(1)))
*/

  def mirror_tree [A](t:Tree[A]) : Tree[A] =
    throw new Exception("TBI")

  val t4 = new Node ('a', new Leaf('b'), new Node ('c',new Leaf('e'),new Leaf ('f')))

/* (d) a function that would return a tree with its
       sized information for each sub-trees
       tagged to each of its elements
Node ((4, 5), Node ((3, 3), Leaf (1, 1), Leaf (1, 2)), Leaf (1, 6))
# t4
- : t4: Node[Char] = Node(a,Leaf(b),Node(c,Leaf(e),Leaf(f)))

# add_size (t4)
- : Node[(Int,Char)] = Node((5,a),Leaf(1,b),Node((3,c),Leaf(1,e),Leaf(1,f)))

*/

  def add_size [A](t:Tree[A]) : Tree[(Int,A)] =
    throw new Exception("TBI")


/* (e) Write a function to check if a tree of integer is
       actually a binary search tree where all the elements are sorted
       in the following ordering. All elements in the left subtree
       are strictly smaller than the root node, which is in turn
       smaller or equal to all elements in the right sub-tree.

# t1
- : Node[Int] = Node(3,Leaf(1),Leaf(2))
# check_bst (t1)
- : Boolean = false
# t5
- : Node[Int] = Node(2,Leaf(1),Leaf(3))
# check_bst (t5)
- : Boolean = true
*/

  val t5 = new Node(2,new Leaf (1), new Leaf (3))

  def check_bst (t:Tree[Int]):Boolean =
    throw new Exception("TBI")

/*
  Q5 : The fold_tree operation uses tree recursion.

      Let us write a different fold tree operation that works
      with the help of accumulating parameter that would
      be similar to List#foldLeft.

      An example of this which uses pre-order traversal is
      given below.
*/

  def fold_tree_preorder [Z,A](f:(Z,A)=>Z) (z:Z) (t:Tree[A]) : Z =
    t match {
      case Leaf(value) => f(z, value)
      case Node(value , lt, rt) => {
            val z1 = f(z,value)
            val z2 = fold_tree_preorder (f) (z1) (lt)
            fold_tree_preorder (f) (z2) (rt)

          }
      }

/* Using fold_tree_preorder, write two functions below */
/*
(a) count the number of elements in the tree
Example:
# t4
- : t4: Node[Char] = Node(a,Leaf(b),Node(c,Leaf(e),Leaf(f)))
# count_tree (t4)
- : Int = 5
*/

  def count_tree [A](t:Tree[A]) : Int =
    throw new Exception("TBI with fold_tree_preorder")


/* (b) Compare this fold_tree_preorder with fold_tree.
   Can one be implemented in terms of the other, or
   are they incomparable?
*/


/*

	Q6: Pretty printers.

	Consider the binary tree defined earlier.

	You have been given a higher-order printer which prints the tree in a pre-fix form.
	As an example, the tree t2 would be printed as:

Node 4
Node 3
Leaf 1
Leaf 2
Node 3
Leaf 1
Leaf 2

	(i) pretty printer
	This above printing is however less readable and you are asked to provide
	a neater printer that would provide space indentation to represent
	the depth of each subtrees.

	Implement pr_tree2, so that it would provide such space indentation
	for each new level of the subtrees, as illustrated below:

Node 4
 Node 3
  Leaf 1
  Leaf 2
 Node 3
  Leaf 1
  Leaf 2

	(i) infix printer.
	One may prefer a tree printer that is presented in an infix manner.
	Write a new pr_tree_infix method that would allow your binary tree to be printed
	in an infix order. The output for t2 example is illustrated below:

  Leaf 1
 Node 3
  Leaf 2
Node 4
  Leaf 1
 Node 3
  Leaf 2

*/

  def pr_tree [A](pr:A=>String) (xs:Tree[A]) : String =
    xs match{
      case Leaf(value) => "Leaf "+ pr(value) +"\n"
      case Node(value , lt, rt) => "Node "+ pr(value) + "\n" + pr_tree(pr)(lt) + pr_tree(pr)(rt)
  }


/* please change Exception .. to your implementation */
  def pr_tree2 [A](pr:A=>String) (xs:Tree[A]) : String
	= throw new Exception("neat tree printer with nested indentation")

/* please change Exception .. to your implementation */
  def pr_tree_infix [A](pr:A=>String) (xs:Tree[A]) : String
	= throw new Exception( "neat tree printer with nested indentation in infix form")

  def intToString(i: Int) = i.toString
	println (pr_tree (intToString) (t3))
	//println (pr_tree2 (intToString) (t3))
	//println (pr_tree_infix (intToString) (t3))

/*

	Q7: Numbered List.

	You have been previously given a printer for lists.

	pr_list2 (pr_id) (ls) ==>
	- : String = "[This; is; a; numbered; list]"

	You have been asked to write a list printer that would number each
	element of its list. Your new function pr_list_num  should result in
	the following:

  pr_list_num ("; ") (pr_id) (ls) ==>
  - : String = "[(1)This; (2)is; (3)a; (4)numbered; (5)list]"

  You may use the add_num method below which adds a number to each element
	of its list.
*/

  def pr_list2 [A](pr:A=>String) (xs:List[A]) : String
  = "[" + xs.map(pr).mkString("; ") + "]"

  def add_num [A](xs:List[A]) : List[(Int,A)] ={
    def aux (xs:List[A]) (n:Int):List[(Int,A)] =
       xs match{
        case Nil => List()
        case x::xs => (n,x)::(aux (xs) (n+1))
    }
    aux (xs) (1)
  }

  val ls = List("This","is","a","numbered","list")

  def pr_id [A](x:A) = x.toString()

  println (pr_list2 (pr_id) (ls))

  def pr_list_num [A](sep:String) (pr:A=>String) (xs: List[A]) : String
  = throw new Exception("to be implemented")

  def test_num [A](sep:String) (pr:A=>String) (xs: List[A]) ={
    val s = pr_list_num (sep) (pr_id) (xs)
    println (s)
  }

/*

  Q8. Higher-order wrappers.

	   These are great for modifying the tracing and monitoring
	   our methods, and could even be used to alter our methods' behaviour.

		 One use of them is to help us perform method call tracing, so
		 as to determine the correctness of our method. A simple tracer for
		 methods is given below which was also applied recursively to the fib method.

		 An example use of tracer is shown below which traced all calls to fib1 4
		 before returning a final result 5.

# fib1 (4)
fib 0 => 1
fib 1 => 1
fib 2 => 2
fib 1 => 1
fib 0 => 1
fib 1 => 1
fib 2 => 2
fib 3 => 3
fib 4 => 5
- : Int = 5

		 Your task is to implement a more selective tracer_test method that
		 takes a predicate, and would only output a call tracing if the predicate
		 holds with the input parameter. An example of its use is below:

     def fib3 n =
		   trace_test ("fib") ((x:Int) => x>1) (intToString) (intToString) (aux) (n)

		 which only output a trace if input x>1. This would rule out printing
		 the base-cases of x<=1 leading to tracing below:

# fib3 4;;
fib 2 => 2
fib 2 => 2
fib 3 => 3
fib 4 => 5
- : int = 5

     Please modify tracer_test to achieve a more selective tracing of
		 the calls.

*/

  def wrapper [A,V,B](pre:A=>V,post:(V,B)=>Unit,post_exc: (V,Exception)=>Unit)
    (f:A=>B) (x:A) : B = {
      val v = pre(x)
      try{
        val r = f(x)
        post (v,r)
        return r
      }
      catch{
        case e: Exception =>{ post_exc(v,e)
                              throw e
                            }
      }
  }

  /* function tracing */
  def tracer [A,B](fn_str:String) (pr_arg:A=>String)
   (pr_res:B=>String) (f:A=>B) (x:A) =
     {
       def pre(x:A) = (fn_str + " " + (pr_arg (x)))
       def post(v:String,r:B) = println (v+" => "+(pr_res(r)))
       def postEx(v:String,e:Exception) = println (v+" => Exception")
       wrapper (pre,post,postEx) (f) (x)
    }

  /* non-recursive tracing of just the first call */
  def fib (n:Int):Int =
    if (n<=1) 1 else fib (n-1)+(fib(n-2))

  def fib1 (n:Int) =
    tracer ("fib") (intToString) (intToString) (fib) (n)

  /* recursive tracing of all calls */
  def fib2 (n:Int):Int ={
      def aux (n:Int) =
        if (n<=1)  1 else fib2 (n-1)+(fib2(n-2))
      tracer ("fib") (intToString) (intToString) (aux) (n)
    }


  /* selective function tracing */
  def trace_test [A,B](fn_str:String) (pr_test:A=>Boolean)
      (pr_arg:A=>String) (pr_res:B=>String) (f:A=>B) (x:A) =
    throw new Exception ("use wrapper to implement a selective function tracing")

/* selective tracing of calls */
  def fib3 (n:Int):Int ={
    def pr_test(x:Int) = x>1
    def aux (n:Int) = if (n<=1)  1 else fib3 (n-1)+(fib3(n-2))
    trace_test ("fib") (pr_test) (intToString) (intToString) (aux) (n)

  }
}
