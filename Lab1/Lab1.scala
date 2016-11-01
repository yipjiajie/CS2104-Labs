object Lab1 {

/*
   Deadline : Please submit to IVLE workbin by
         6pm 2Sept2016 (Friday)

  Q1 : Write a recursive function that would return the
       last element of the list. In case of empty list,
       throw an exception with "Failure" message.

       What is the polymorphic type of this function?*/
  
  def last[A](xs:List[A]):A = 
    throw new Exception("To be implemented")
  
/*  Q2 : Change the last() function to one with the following
       type: List[A] => Option[A]
       This function should return Some(v), where v is the
       last element of the list. If the list is empty, you
       should return None.
*/
  
  def last_opt[A](l:List[A]): Option[A] = 
    throw new Exception("To be implemented")
  
/*   Q3 : Write a recursive function that would return the
       last two elements of the lists as a pair of values.
       In case you have less than two elements, throw a Failure exception.

       What is the polymorphic type of this function?
*/
  
  def last_two[A](l:List[A]):(A,A) = 
    throw new Exception("To be implemented")
  
  
/*  Q4 : Write a recursive function to sort a list of numbers
       using the insertion sort method.

       For your convenience, we have provided an
       insert procedure.
       (i) can you improve the insert method to
           avoid constructing (y::ys) in the base case?
           (Hint : use the alias @-pattern notation)
      (ii) implement a recursive sort method*/

  def insert(x: Int, ys: List[Int]) : List[Int] =
    ys match {
     case y :: ys =>
       if (x < y) x :: y :: ys
       else y::(insert(x,ys))
     case Nil => List(x)
    }
  
  def sort(xs:List[Int]) = 
    throw new Exception("To be implemented")
/*
  Q4 : Consider a Uprim type to capture either 
       integer, float or a string value.

       You can build a list of mixed type using
       it, and can perform (List)reverse and (List)length
       using it.

       Compute the sum of mixed list using the value_of_mix
       function.*/
       
sealed trait Uprim
case class I(i: Int) extends Uprim
case class F(f: Float) extends Uprim
case class S(s: String) extends Uprim

val mix_ls:List[Uprim] = List(I(3), F(4.3f), S("hello"), I(4))

println("mix_ls has length "+(mix_ls.length))

val mix_ls1 = mix_ls.reverse
println(mix_ls1)

def value_of_mix(up: Uprim): Int =
    up match {
    case I(i) => i
    case F(f) => f.toInt
    case S(s) => s.length
    }

def sum_of_mix_list(ms:List[Uprim]):Int = 
    throw new Exception("To be implemented")
  
/*       

  Q5 : Let us define Uprim using the basic sum type instead,
       and write functions that are isormoprhic to those
       found in Q4. */

sealed trait sum[A, B]
final case class L[A, B](a: A) extends sum[A, B]
final case class R[A, B](b: B) extends sum[A, B]

type Uprim2 = sum[Int,sum[Float,String]]

def mk_I(v:Int):Uprim2 = L(v)
def mk_F(f:Float):Uprim2 = R(L(f))
def mk_S(s:String):Uprim2 = R(R(s))

val mix_ls2 = List(mk_I(3), mk_F(4.3f), mk_S("hello"), mk_I(4))

println("mix_ls2 has length "+(mix_ls2.length))
val mix_ls2r = mix_ls2.reverse

def value_of_mix2(up:Uprim2) = up match{
  case L(v) => v
  case R(L(v)) => v.toInt /*truncates the float value*/
  case R(R(s)) => s.length() /*length of string*/
}

def sum_of_mix_list2 (ms:List[Uprim2]):Int = 
    throw new Exception("To be implemented")


/*
  Q6 : Consider a polymorphic tree.

       Write a function that will return the largest value in
       the tree. You may use the scala.math.max function. */

sealed trait btree[A]
case class Node[A](n: A, l:btree[A], r:btree[A]) extends btree[A]
case class Leaf[A](n:A) extends btree[A]

var t1 = Leaf(3)
var t2 = Node(4,t1,t1)
t2 = Node(6,t2,t1)

def max_tree(t: btree[Int] ) : Int = 
  throw new Exception("To be implemented")


/*  
	Q7 : Below is a function that will flatten a tree into a list
       by traversing the tree in an infix-order.

       Write another function that will flatten a tree 
       based on pre-fix traversal.*/

def flatten_infix[A](t: btree[A]): List[A] = t match {
  case Leaf(v) => List(v)
  case Node(v,lt,rt) => (flatten_infix(lt)) ++ List(v) ++ (flatten_infix(rt))  
}

def flatten_prefix[A](t: btree[A]): List[A] ={
  def qux[A](t: btree[A]):List[A] = t match{
    case Leaf(v) => List(v)
    case Node(v,lt,rt) => throw new Exception("To be implemented")
  }
  qux(t)
}


/*
   Q8 : The power function takes two arguments x n so as
       to return x^n.

       An expected precondition is that n>=0
       Write an assertion statement to ensure that this pre-condition
       will always be met.

       What happens to your function if you had used
       a negative n value?*/

def power(x: Int, n: Int): Int =
  if (n == 0) 1
  else x*(power(x,n-1))


/*
Q9 : 

       The above code below merely expresses the fact that
         power x 0 = 1
         power x n = x * (power (n-1))

       The above function is NOT tail-recursive.
       Can you write a tail-recursive
       version of this function which would accumulate its
       result in a 3rd paramater, called acc, as shown below?
*/
  
def power_tail(x: Int, n: Int): Int ={
  def aux[A<:Int](x: A, n: A, acc: A) =
      throw new Exception("To be implemented")
  aux(x,n,1)
}


/* 
  Q10 : 
       We can also get a logarithmic-time function using
         power x 0 = 1
         power x (2*n = power (x^2) n
         power x (2*n+1) = x*(power (x^2) n)
       Implement such a function tail-recursively.
       How does this compare with the cryptic version of the code
       shown in Lecture 1.
*/

def power_logn(x: Int, n: Int): Int ={
  def aux[A<:Int](x: A, n: A, acc: A) =
    throw new Exception("To be implemented")
  aux(x,n,1)
}

def main(agrs: Array[String]){
  }
}