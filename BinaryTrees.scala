package Main

object BinaryTrees {
  def main(args: Array[String]): Unit = {
    println("Tree1 is: " + tree1)
    println("Tree2 is: " + tree2)
  }
    val tree1 = new NonEmptyTree(3, new EmptyTree,new EmptyTree)
    val tree2 = tree1 includes 1
 
}

abstract class IntBinaryTree {
  def contains(x:Int): Boolean
  def includes(x:Int): IntBinaryTree
}

//Or can use object for Singleton object
class EmptyTree extends IntBinaryTree {
  def contains(x:Int) : Boolean  = false
  def includes(x:Int): IntBinaryTree = new NonEmptyTree(x,new EmptyTree, new EmptyTree)
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
  override def toString = "{" + leftBranch + element + rightBranch +  "}"
}
