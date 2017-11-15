package Main

object BinaryTrees {
  def main(args: Array[String]): Unit = {
    println("Tree1 is: " + tree1)
    println("Tree2 is: " + tree2)
    println("******************************************************")
    println("Tree4 is: " + tree4)
  }
    val tree1 = new NonEmptyTree(3, new EmptyTree,new EmptyTree)
    val tree2 = tree1 includes 1
    val tree3 = tree1 union tree2
    val tree4 = tree2 union tree3 includes 5
}

abstract class IntBinaryTree {
  def contains(x:Int): Boolean
  def includes(x:Int): IntBinaryTree
  def union(other:IntBinaryTree): IntBinaryTree
}

//Or can use object for Singleton object
class EmptyTree extends IntBinaryTree {
  def contains(x:Int) : Boolean  = false
  def includes(x:Int): IntBinaryTree = new NonEmptyTree(x,new EmptyTree, new EmptyTree)
  def union(other:IntBinaryTree) : IntBinaryTree = other
  override def toString = "."
}

//Or can use object for Singleton object
class NonEmptyTree(element:Int, leftBranch: IntBinaryTree,rightBranch: IntBinaryTree) extends IntBinaryTree {
  def contains(x:Int) : Boolean = {
    if(x<element) leftBranch contains x 
    else if(x>element) rightBranch contains x
    else true
  }
  
  def includes(x:Int) : IntBinaryTree = {
    if(x<element) new NonEmptyTree(element,leftBranch includes x,rightBranch)
    else if (x>element) new NonEmptyTree(element,leftBranch,rightBranch includes x)
    else this
  }
  def union(other:IntBinaryTree) : IntBinaryTree = {
    ((leftBranch union rightBranch) union other) includes element
  }
  override def toString = "{" + leftBranch + element + rightBranch +  "}"
  
}
