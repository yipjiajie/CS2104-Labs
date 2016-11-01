
/*
 *  Ahmet Alparslan Celik
 *  CS2104 - 1
 *  NUSNET ID: E0024907
 */


/****************************************/
/* Lab 2/Tut 4 : Higher-Order Functions */
/****************************************/

/* As there is no tutorial on 12Sept, this exercise will be just a homework */
/* This lab assignment is to be submitted by 28th Sept 2016 6pm */
/* In case you have problems, please email your lecturer or tutor for consultation */

object Lab2 extends App{
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

  /*
      ===== fix a bug in the code below =====

      To fix the bug, the function should join the current element and
      accumulated list not whole list.
  */
  def reverse [X](xs:List[X]) : List[X] =
    xs.foldLeft (List[X]()) ((acc,x)=>(x::acc))

  def print_q1 () =
    println( "Q1 - Fold Left: " + reverse(List(1,2,3)));



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

  def product [A,B](xs:List[A]) (ys:List[B]) : List[(A, B)] =
    xs.map(a => ys.map( b => (a, b) )).flatten

  def print_q2a () =
    println( "Q2a - Cartesian product: " + product(List(5,9,4)) (List(2,0,3)) );

  /*
     (b) cross-product of two lists that returns an integer
         from each list as a pair (a,b) such that a<=b.
         That is [(a,b) | a <- xs, b <- ys, a<=b]
     Example : product2 (List(1,2,3)) (List(2,7))
     ==>
  - : List[Int,Int] = List((1, 2), (1, 7), (2, 2), (2, 7), (3, 7))
  */

  def product2(xs:List[Int]) (ys:List[Int]) =
    product(xs)(ys).filter( p => (p._1 <= p._2) )

  def print_q2b () =
    println( "Q2b - Cartesian product: " + product2 (List(1,2,3)) (List(2,7)) )

  /*
     (c) The divisor from a product of two lists, but only
         if the second element is non-zero.
         That is [a/b | b<-ys, b!=0, a <- xs]
     Example : divisor_prod (List(5,9,4)) (List(2,0,3))
     ==>
     - :  List[Int] = List(2, 4, 2, 1, 3, 1)
  */

  def divisor_prod(xs:List[Int]) (ys:List[Int])  =
    ys.map(y => xs.map( x => (x, y) )).flatten.filter( _._2 != 0).map( p => (p._1 / p._2))

  def print_q2c () =
    println( "Q2c - Divisor from a product: " + divisor_prod (List(5,9,4)) (List(2,0,3)) )

  def print_q2 () = {
    print_q2a()
    print_q2b()
    print_q2c()
  }

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
    ys.foldRight((0, 0, 0))((a,b) => {
      b match {
        case (b1,b2,b3) => a compare 0 match {
          case 0 => (b1, b2, b3 + 1)
          case -1 =>  (b1, b2 + 1, b3)
          case 1 => (b1 + 1, b2, b3)
        }
      }
    })

  def count_nums2 (ys:List[Int]) : (Int, Int, Int) =
    (ys.filter(x => (x > 0)).length, ys.filter(x => (x < 0)).length, ys.filter(x => (x == 0)).length)

  def print_q3 () =
    println( "Q3 - Count numbers part 1: " + count_nums1 (List(1,-4,5,8,0,-9)) + "\n" +
      "Q3 - Count numbers part 2: " + count_nums2 (List(1,-4,5,8,0,-9)) )

  /*
    Q4 : Higher-Order functions for Trees

    During the last tutorial, we implemented two higher-order
          functions for a simple binary trees as follows:

    Let us practise more examples on them.
  */
  abstract class Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  lazy val t1 = new Node (3,new Leaf(1), new Leaf(2));;
  lazy val t2 = new Node (4,t1,new Leaf (6));;
  lazy val t3 = new Node (5,t2,new Leaf (3));;

  /* map for tree */
  def map_tree [A,B] (f:A=>B) (tree: Tree[A]) : Tree[B] = tree match {
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
    map_tree[Int, Int](v => v + n)(t)

  def print_q4a () : Unit =
    println("Q4a - Adding n to every element of a tree: " + add_n(t2,3))

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
    fold_tree[A, A](x => x)((v,l,r) => r)(t)

  def print_q4b () : Unit =
    println("Q4b - Printing right most element of a tree: " + right_most (t2))

  /* (c) a function that would return the mirror of a tree
          where left and right subtrees are recursively flipped

  # t2
  - : Node[Int] = Node(4,Node(3,Leaf(1),Leaf(2)),Leaf(6))
  # mirror_tree (t2)
  - : Node[Int] = Node(4,Leaf(6),Node(3,Leaf(2),Leaf(1)))
  */

  def mirror_tree [A](t:Tree[A]) : Tree[A] =
    fold_tree[A,Tree[A]](x => Leaf(x))((v,l,r) => Node(v, r, l))(t)

  def print_q4c () =
    println( "Q4c - Returning the mirror of a tree: " + mirror_tree(t2))

  lazy val t4 = new Node ('a', new Leaf('b'), new Node ('c',new Leaf('e'),new Leaf ('f')))

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
    fold_tree[A, (Tree[(Int, A)], Int)](
      v => (Leaf(1, v),1))(
      (v, l, r) => (Node((1 + l._2 + r._2, v), l._1, r._1), 1 + l._2 + r._2))(t)._1

  def print_q4d () =
    println( "Q4d - Returning a tree with its size information: " + add_size(t4) )

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

  lazy val t5 = new Node(2,new Leaf (1), new Leaf (3))

  def check_bst (t:Tree[Int]):Boolean =
    fold_tree[Int, (Boolean,Int,Int)](v => (true,v, v))((v, l, r) => (l._1 && r._1 && v > l._3 && v <= r._2, l._2, r._3))(t)._1

  def print_q4e () =
    println( "Q4e - Checking a binary tree is a actual BTS: " + check_bst(t5))

  def print_q4 (): Unit = {
    print_q4a()
    print_q4b()
    print_q4c()
    print_q4d()
    print_q4e()
  }
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
    fold_tree_preorder[Int,A]((n,m) => n+1)(0)(t)

  def print_q5 () =
    println("Q5a - Counting the number of elements in a tree: " + count_tree (t4))

  /* (b) Compare this fold_tree_preorder with fold_tree.
     Can one be implemented in terms of the other, or
     are they incomparable?
  */
  /*
      fold_tree_preorder function performs a pre-order traversal. This means that
        1) it display the value of the root/current node
        2) it traverses left sub-tree by recursively calling the pre-order function
        3) it traverses right sub-tree by recursively calling the pre-order function

      fold_tree function performs a post-order traversal. This means that
        1) it traverses the left sub-tree by recursively calling the post-order function
        2) it traverses right sub-tree by recursively calling the post-order function
        3) it display the value of the root/current node

      Further differences are the following. fold_tree uses two separate functions as its
      paramaters but fold_tree_preorder uses one function as a parameter. Therefore, it is
      not wrong to say that fold_tree is more general than fold_tree_preorder. Consequtively,
      it is possible to implement fold_tree_preorder function in terms of fold_tree but not
      vice versa.

      To understand the implementation, we should look at the difference between functions.

      Lets assume, we are working on the tree below:
          Val1
            Val2
              Val4
              Val5
            Val3

      Lets denote fold_tree_preorder parameter as "f". Then, I can rewrite the fold_tree_preorder
      function as the following:
              f(f(f(f(f(z,val1),val2),val4),val5), val3)

      Lets denote fold_tree parameter as "f1" and "f2". Also I can rewrite the fold_tree function as the following:
              f2(val1,f2(val2,f1(val4),f1(val5)),f1(val3))

      To implement fold_tree_preorder in terms of fold_tree, we should return a function type.
      After each operation, the function returned from fold_tree operations will be composed a
      nested function which is similar to (*).
      (*)     f(f(f(f(f(x,val1),val2),val4),val5), val3)  // x is unknown

      Then we apply the inital value to the function which is z and we get (-).
      (-)     f(f(f(f(f(z,val1),val2),val4),val5), val3)

      Therefore, we show that we can implement fold_tree_preorder in terms of fold_tree.
      An example implementation is provided below.

  */

def fold_tree_preorder_via_fold_tree [Z,A](f:(Z,A)=>Z) (z:Z) (t:Tree[A]) : Z = {
    def f1 (a:A) : (Z=>Z) = ((m:Z) => f(m,a))
    def f2 (a:A, l: Z=>Z, r: Z=>Z) : (Z=>Z) =
      r compose (l compose f1(a))
    fold_tree(f1)(f2)(t)(z)
  }
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
  def pr_tree2 [A](pr:A=>String) (xs:Tree[A]) : String = {
    def aux[A](pr:A=>String)(w:Int)(xs:Tree[A]) : String =
      xs match{
        case Leaf(value) => " " * w + "Leaf "+ pr(value) + "\n"
        case Node(value , lt, rt) => " " * w + "Node "+ pr(value) + "\n" + aux(pr)(w + 1)(lt) + aux(pr)(w + 1)(rt)
      }
    aux(pr)(0)(xs)
  }

  /* please change Exception .. to your implementation */
  def pr_tree_infix [A](pr:A=>String) (xs:Tree[A]) : String = {
    def aux[A](pr:A=>String)(w:Int)(xs:Tree[A]) : String =
      xs match{
        case Leaf(value) => " " * w + "Leaf "+ pr(value) + "\n"
        case Node(value , lt, rt) => aux(pr)(w + 1)(lt) + " " * w + "Node "+ pr(value) + "\n" + aux(pr)(w + 1)(rt)
      }
    aux(pr)(0)(xs)
  }

  def intToString(i: Int) = i.toString

  def print_q6 () = {
    println( "Q6 - Tree printer in prefix form: \n" + pr_tree (intToString) (t3) )
    println( "Q6 - Neat tree printer with nested indentation in prefix form: \n" + pr_tree2(intToString)(t3) )
    println( "Q6 - Neat tree printer with nested indentation in infix form: \n" + pr_tree_infix (intToString) (t3))
  }
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

  lazy val ls = List("This","is","a","numbered","list")

  def pr_id [A](x:A) : String = x.toString()

  def pr_list_num [A](sep:String) (pr:A=>String) (xs: List[A]) : String = {
    def pr_tup[A](t:(Int,A)) : String = "(" + t._1.toString + ")" + t._2.toString

    "[" + add_num(xs.map(pr)).map(pr_tup).mkString("; ") + "]"
  }

  def test_num [A](sep:String) (pr:A=>String) (xs: List[A]) ={
    val s = pr_list_num (sep) (pr_id) (xs)
    println (s)
  }

  def print_q7 () = {
    print( "Q7 - Numbered list printer: " )
    test_num("; ")(pr_id)(ls)
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
      case e: Exception => {
        post_exc(v,e)
        throw e
      }
    }
  }

  /* function tracing */
  def tracer [A,B](fn_str:String) (pr_arg:A=>String)
                  (pr_res:B=>String) (f:A=>B) (x:A) =
  {
    def pre(x:A) = (fn_str + " " + (pr_arg (x)))
    def post(v:String,r:B) = println( v + " => "+ (pr_res(r)) )
    def postEx(v:String,e:Exception) = println (v+" => Exception")
    wrapper (pre,post,postEx) (f) (x)
  }

  /* non-recursive tracing of just the first call */
  def fib (n:Int):Int =
    if (n<=1) 1 else fib (n-1)+(fib(n-2))

  def fib1 (n:Int) =
    tracer ("fib") (intToString) (intToString) (fib) (n)

  /* recursive tracing of all calls */
  def fib2 (n:Int):Int = {
    def aux (n:Int) =
      if (n<=1)  1 else fib2 (n-1)+(fib2(n-2))
    tracer ("fib") (intToString) (intToString) (aux) (n)
  }

  /*
      ====== selective function tracing ======

      In order to make the function more general and elegant, Option type is used.

      Roughly explaining, pre function checks the testing expression,
      if it is correct it return Some(String) else None. Then when the post
      function is called, it will print the trace string unless the returned
      type is None.
   */
  def trace_test [A,B](fn_str:String) (pr_test:A=>Boolean)
                      (pr_arg:A=>String) (pr_res:B=>String) (f:A=>B) (x:A) = {
    def pre(x: A) = if(pr_test(x)) Some(fn_str + " " + (pr_arg(x))) else None
    def post(v: Option[String], r: B) = if(v != None) println(v + " => " + (pr_res(r)))
    def postEx(v: Option[String], e: Exception) = if(v != None) println(Some(v) + " => Exception")

    wrapper(pre, post, postEx)(f)(x)
  }

  /* selective tracing of calls */
  def fib3 (n:Int):Int = {
    def pr_test(x:Int) = x > 1
    def aux (n:Int) = if (n<=1)  1 else fib3 (n-1)+(fib3(n-2))
    trace_test ("fib") (pr_test) (intToString) (intToString) (aux) (n)
  }

  def print_q8 () = {
    println( "Q8 - Selective function tracing: " )
    fib3(4)
  }

  override def main(agrs: Array[String]): Unit = {
    print_q1()
    print_q2()
    print_q3()
    print_q4()
    print_q5()
    print_q6()
    print_q7()
    print_q8()
  }
}
