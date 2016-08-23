//package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {




}

def maximum(tree: Tree[Int]): Int =
	tree match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max maximum(r)
	}

def depth[A](tree: Tree[A]): Int =
	tree match {
		case Leaf(v) => 1
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
	tree match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = 
	t match {
		case Leaf(v) => f(v)
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

def sizeFold[A](t: Tree[A]): Int =
	fold(t)(_ => 1)(1 + _ + _)

def maximumFold(t: Tree[Int]): Int =
	fold(t)(v => v)(_ max _)

def depthFold(t: Tree[Int]): Int =
	fold(t)(v => 1)(_ max _ + 1)

def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
	fold[A, Tree[B]](t)(v => Leaf(f(v)))((l, r) => Branch(l, r))	

val tree1 = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
val tree2 = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))