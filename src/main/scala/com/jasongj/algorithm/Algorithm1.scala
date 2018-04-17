package com.jasongj.algorithm

import org.testng.annotations.Test



class Algorithm1 {

  /*
  斐波纳契数
  动态规划法
   */
  @Test
  def fibonaci: Unit = {
    val n = 10
    val dp = new Array[Int](n)
    dp(0) = 1
    dp(1) = 1
    def cal(xx : Int) : Int = {
      if(xx <= 1) dp(xx)
      else if ( dp(xx - 1) == 0) {
        dp(xx-1) = cal(xx-1)
      } else if( dp(xx-2) == 0) {
        dp(xx-2) = cal(xx-2)
      }
      dp(xx-1) + dp(xx-2)
    }
    println(cal(6))
  }

  /*
  斐波纳契数
  自底向上 递推法
  */
  @Test
  def fibonaciDownToTop: Unit = {
    def cal (current : Int, target : Int, first : Int, second : Int) : Int = {
      if(current == target) first + second
      else cal(current+1, target, second, first + second)
    }

    println(cal(2, 8, 1, 1))
  }

  /*
  二维矩阵 左上角到右下角最短路径 （每次只能向右或向下移动一步）
   */
  @Test
  def minPath(): Unit = {
    val data = Array(Array(1,3,5,9),Array(8,1,3,4),Array(5,0,6,1),Array(8,8,4,0))
    val path = Array.ofDim[Int](4,4)
    for (i <- 0 to data.length - 1) {
      for (j <- 0 to data(i).length - 1) {
        path(i)(j) = data(i)(j) + ((i, j) match {
          case (i, j) if (i==0 && j==0) => 0
          case (i, j) if (i==0 && j!=0) => path(i)(j-1)
          case (i, j) if (i!=0 && j==0) => path(i-1)(j)
          case (i, j) => Math.min(path(i)(j-1), path(i-1)(j))
        })
      }
    }
    println(path(3)(3))

//    path.foreach((arr) => arr.foreach(println(_)))

    var x=3
    var y=3
    println(data(x)(y))
    data.foreach(row => {row.foreach(printf("%3s",_)); println()})
  }

  /**
    * 最大上升子串
    */
  @Test
  def lis = {

    val data = Array(1,2,0,3,6,4,8,9,7)
    val dp = new Array[Int](data.length)
    val abc = new Array[Int](10)
    for (i <- 0 to data.length - 1) dp(i) = 1
    for (i <- 0 to abc.length - 1) abc(i) = 0
    var max : Int = 1;
    for (i <- 0 to data.length - 1) {
      for ( j <- 0 to i - 1) {
        if(data(i) > data(j) && dp(j) + 1 > dp(i)) dp(i) = dp(j) + 1
      }
      if(dp(i) > max) max = dp(i)
      if(abc(dp(i)-1) < data(i)) abc(dp(i)-1) = data(i)
    }
    println(max)

  }

  /*
    0-1 背包问题
    动态规划 二维数组
   */
  @Test
  def bag_0_1 = {
//    val values = Array[Int](42,12,40,25)
//    val weights = Array[Int](7,3,4,5)
    val values = Array[Int](8,10,6,3,7,2)
    val weights = Array[Int](4,6,2,2,5,1)
    val n = values.length
    val cap = 12
    val dp = Array.ofDim[Int](n, cap+1)
    val bag = new Array[Int](n)

    for ( i <- 0 to n - 1) {
      for ( j <- 0 to cap ) {
        if (i == 0) dp(i)(j) = if( j < weights(i)) 0 else values(i)
        else dp(i)(j) = if(j >= weights(i)) Math.max(dp(i-1)(j), dp(i-1)(j- weights(i)) + values(i)) else dp(i-1)(j)
      }
    }

    println(dp(n-1)(cap))

    dp.foreach(row => {row.foreach(printf("%2s ", _)); println(""); })

    var max = cap
    for ( i <- n - 1 to 1 by -1) {
      if (dp(i)(max) != dp(i-1)(max)) {
        bag(i) = 1
        max -= weights(i)
      }
    }
    bag(0) = if(dp(0)(max) > 0)  1 else 0

    bag.foreach(printf("%2s ", _))


  }

  /*
    0-1 背包问题
    动态规划，压缩规划表（使用一维数组）
   */
  @Test
  def bag_0_1_compress = {
    //    val values = Array[Int](42,12,40,25)
    //    val weights = Array[Int](7,3,4,5)
    val values = Array[Int](8,10,6,3,7,2)
    val weights = Array[Int](4,6,2,2,5,1)
    val n = values.length
    val cap = 12
    val dp = new Array[Int](cap + 1)
    val bag = new Array[Int](n)

    for ( i <- 0 to n - 1) {
      for ( j <- cap to 0 by -1 ) {
        if(i == 0) dp(j) = if(j < weights(i)) 0 else values(i)
        else if(j >= weights(i)) dp(j) = Math.max(dp( j - weights(i)) + values(i), dp(j))
      }
    }

    println(dp(cap))

    dp.foreach(printf("%2s ", _))



  }

  /*
    0-1 背包问题
    自顶向下，递归，重复计算
   */
  @Test
  def bag_0_1_Recursive = {
    val values = Array[Int](8,10,6,3,7,2)
    val weights = Array[Int](4,6,2,2,5,1)
    val n = values.length
    val cap = 12
    val dp = Array.ofDim[Int](n, cap+1)
    val bag = new Array[Int](n)


    def find( total : Int, capacity : Int): Int = {
      if(total == 0 || capacity == 0 ) {
        0
      } else {
        val i = total - 1
        if(weights(i) > capacity) {
          bag(i)  = 0
          find( total - 1, capacity);
        } else {
          val tmp1 = find( total - 1, capacity)
          val tmp2 = find( total - 1, capacity - weights(i)) + values(i)
          if(tmp1 > tmp2) {
            bag(i) = 0
            tmp1
          } else {
            bag(i) = 1
            tmp2
          }
        }
      }
    }

    println(find(n, cap))
    bag.foreach(printf("%2s ", _))

  }

  /*
   长度为n的数组，元素范围[0,n-1]，找出重复数字
   */
  @Test
  def duplicatedNum = {
    val data = Array[Int](1,3,5,9,2,8,9,7,9,4,10)
    var dup = -1;
    def getDumplicatedNum : Int = {
      for( i <- 0 to data.length - 1) {
        while(data(i) != i+1) {
          if(data(data(i) - 1) == data(i)) return data(i)
          val tmp = data(data(i) - 1)
          data(data(i) - 1) = data(i)
          data(i) = tmp
        }
      }
      dup
    }
    println(getDumplicatedNum)
  }

  /*
    找出字符串中最长的无重复字符的子串
   */
  @Test
  def getUnDeplicatedSubStr =  {
    val data = "abcdeacdfbg"
    val map = collection.mutable.Map.empty[Character, Int]
    var head = 0
    var max = 0
    var first = 0
    var second = 0
    for (i <- 0 to data.length() - 1) {
      val value = data.charAt(i)
      if(map.contains(value)) {
        head = Math.max(head, map.get(value).get + 1)
      }
      map.put(value, i)
      if(i - head + 1 > max) {
        max = i - head + 1
        first = head
        second = i
      }

      max = Math.max(max, i - head + 1)
    }
    println(max)
    println(data.substring(first, second + 1))

  }

  /*
    找出树中路径和等于指定数字的路径
   */
  @Test
  def treePathSum = {
    import scala.collection.mutable.Stack
    import java.util.{ArrayList, List}

    case class TreeNode (var value : Int, var left : TreeNode, var right : TreeNode){
//      val value : Int = _value;
//      var left : TreeNode = _left;
//      var right : TreeNode = _right;
    }
    val path : Stack[TreeNode] = new Stack[TreeNode]()
    val result : List[Array[TreeNode]] = new ArrayList[Array[TreeNode]]()
    def getSum(root : TreeNode, sum : Int, path : Stack[TreeNode]) : Unit = {
      if(root == null) return
      path.push(root)
      if(root.left == null && root.right == null) {
        if(root.value == sum) {
          result.add(path.toArray.reverse)
//          path.pop()
        }
      }
      if(root.left != null) {
        getSum(root.left, sum - root.value, path)
        path.pop()
      }

      if(root.right != null) {
        getSum(root.right, sum - root.value, path)
        path.pop()
      }
    }

    val root = TreeNode(3, TreeNode(5, TreeNode(6, null, null), TreeNode(8, null, null)), TreeNode(7, TreeNode(5, null, null), TreeNode(4, null,null)))
    getSum(root, 14, path)


    for (i <- 0 to result.size() - 1) {
      val row = result.get(i)
      row.foreach(arr => printf("%2s ", arr.value))
      println()
    }
    /*
              3
          5        7
      6      8   5    4
     */

  }

  /*
    找出矩阵（图）中，与指定位置连通的区域面积
    使用栈，而非递归，且深度优先
   */
  @Test
  def  joinedSubDFS = {
    /*
    0  1  1  1  0
    1  1  0  1  1
    1  0  1  0  0
    1  1  1  1  0
     */
    val data = Array(Array(0, 1, 1, 0, 0),Array(1,1,0,1,0),Array(1,0,1,0,0),Array(1,1,1,0,0))
    data.foreach(row => {row.foreach(printf("%2s ",_)); println()})

    def getMax(data : Array[Array[Int]], x : Int, y : Int): Int = {
      import scala.collection.mutable.Stack
      val stack : Stack[(Int, Int)] = new Stack[(Int, Int)]
      if(data == null || data(0) == null || x < 0 || y < 0 || x >= data.length || y >= data(0).length) {
        -1
      }

      val target = data(x)(y)
      stack.push((x, y));
      data(x)(y) = 2
      var max : Int = 0
//      var xx = -1
//      var yy = -1
      def mark (stack : Stack[(Int, Int)], xx : Int, yy : Int, target : Int) : Unit = {
        if(xx >= 0 && yy >= 0 && xx < data.length && yy < data(0).length && data(xx)(yy) == target) {
          stack.push((xx, yy))
          data(xx)(yy) = 2
        }
      }
      while (!stack.isEmpty) {
//        var xx : Int = _
//        var yy : Int = _
        val (xx, yy) = stack.pop
        max += 1

        mark(stack, xx-1, yy, target)
        mark(stack, xx, yy-1, target)
        mark(stack, xx+1, yy, target)
        mark(stack, xx, yy+1, target)

      }
      data.foreach(row => {row.foreach(printf("%2s", _)); println()})
      max
    }
    println(getMax(data, 0, 3))

  }


  /*
    找出矩阵（图）中，与指定位置连通的区域面积
    使用队列，而非递归，且广度优先
   */
  @Test
  def  joinedSubBFS = {
    /*
    0  1  1  1  0
    1  1  0  1  1
    1  0  1  0  0
    1  1  1  1  0
     */
    val data = Array(Array(0, 1, 1, 0, 0),Array(1,1,0,1,0),Array(1,0,1,0,0),Array(1,1,1,0,0))
    data.foreach(row => {row.foreach(printf("%2s ",_)); println()})

    def getMax(data : Array[Array[Int]], x : Int, y : Int): Int = {
      import scala.collection.mutable.Queue
      val queue : Queue[(Int, Int)] = new Queue[(Int, Int)]
      if(data == null || data(0) == null || x < 0 || y < 0 || x >= data.length || y >= data(0).length) {
        -1
      }

      val target = data(x)(y)
      queue.enqueue((x, y));
      data(x)(y) = 2
      var max : Int = 0
      //      var xx = -1
      //      var yy = -1
      def mark (queue : Queue[(Int, Int)], xx : Int, yy : Int, target : Int) : Unit = {
        if(xx >= 0 && yy >= 0 && xx < data.length && yy < data(0).length && data(xx)(yy) == target) {
          queue.enqueue((xx, yy))
          data(xx)(yy) = 2
        }
      }
      while (!queue.isEmpty) {
        //        var xx : Int = _
        //        var yy : Int = _
        val (xx, yy) = queue.dequeue()
        max += 1

        mark(queue, xx-1, yy, target)
        mark(queue, xx, yy-1, target)
        mark(queue, xx+1, yy, target)
        mark(queue, xx, yy+1, target)

      }
      data.foreach(row => {row.foreach(printf("%2s", _)); println()})
      max
    }
    println(getMax(data, 0, 3))

  }

  /*
    将被1完全围绕的连通0子区域标记成1
   */
  @Test
  def  surroundedSubRegion = {
    /*
    0  1  1  1  0
    1  1  0  1  1
    1  0  0  0  1
    1  1  0  1  0
    1  1  1  1  0
     */
    val data = Array(Array(0, 1, 1, 1, 0),Array(1,1,0,1,1),Array(1,0,0,0,1),Array(1,1,0,1,0),Array(1,1,1,1,0))
    data.foreach(row => {row.foreach(printf("%2s ",_)); println()})

    def markSubRegion(data : Array[Array[Int]]): Unit = {
      import scala.collection.mutable.Queue
      if(data == null || data(0) == null) {
        return
      }

      def mark (queue : Queue[(Int, Int)], xx : Int, yy : Int, target : Int, label : Int) : Unit = {
        if(xx >= 0 && yy >= 0 && xx < data.length && yy < data(0).length && data(xx)(yy) == target) {
          queue.enqueue((xx, yy))
          data(xx)(yy) = label
        }
      }

      val queue : Queue[(Int, Int)] = new Queue[(Int, Int)]
      for ( i <- Seq(0, data.length - 1) ; j <- 0 to data(0).length - 1 if data(i)(j) == 0) queue.enqueue((i, j))
      for ( i <- 0 to data.length - 1 ; j <- Seq(0, data(0).length - 1) if data(i)(j) == 0) queue.enqueue((i, j))
      while(!queue.isEmpty) {
        val (xx, yy) = queue.dequeue()
        mark(queue, xx - 1, yy, 0, 2)
        mark(queue, xx, yy - 1, 0, 2)
        mark(queue, xx + 1, yy, 0, 2)
        mark(queue, xx, yy + 1, 0, 2)
      }

      for ( i <- 1 to data.length - 2; j <- 1 to data(0).length - 2 if data(i)(j) == 0) {
        queue.clear();
        mark(queue, i, j, 0, 3)
        while(!queue.isEmpty) {
          val (xx, yy) = queue.dequeue()
          mark(queue, xx - 1, yy, 0, 3)
          mark(queue, xx, yy - 1, 0, 3)
          mark(queue, xx+ 1, yy, 0, 3)
          mark(queue, xx, yy + 1, 0, 3)
        }
      }

      for (i <- 0 to data.length - 1; j <- 0 to data(0).length - 1 if data(i)(j) == 2)
        data(i)(j) = 0

      for (i <- 0 to data.length - 1; j <- 0 to data(0).length - 1 if data(i)(j) == 3)
        data(i)(j) = 1

    }
    markSubRegion(data)
    println()
    data.foreach(row => {row.foreach(printf("%2s ", _)); println()})

  }

  /*
    三角形，从顶点开始，只能向下或右下走，求从顶点到底边（任意一点）的最小路径
   */
  @Test
  def triangle = {
    /*
    7
    3 8
    8 1 0
    2 7 4 4
    4 5 2 6 5
     */
    val data = Array(Array(7), Array(3, 8), Array(8, 1, 0), Array(2, 7, 4, 4), Array(4, 5, 2, 6, 5))
    data.foreach(row => println(row.length))
  }

  @Test
  def test = {
    val data : Int = 0
    println(data.toChar)
  }

  /*
    求岛的个数
    1 为陆，0 为水，被水围绕（或处于矩阵边缘）的陆地即为岛
   */
  @Test
  def numOfIslands(): Unit = {
    /*
      1  0  0  1  0
      0  1  1  0  0
      0  0  1  0  0
      0  1  0  1  0
      1  0  1  0  1
    */
    val data = Array(Array('1','0','0','1','0'),Array('0','1','1','0','0'),Array('0','0','1','0','0'),Array('0','1','0','1','0'),Array('1','0','1','0','1'))
    def numIslands(grid: Array[Array[Char]]): Int = {
      if(grid == null || grid.length == 0 || grid(0) == null || grid(0).length == 0) {
        -1
      }
      var max = 0
      import scala.collection.mutable.Queue
      val queue : Queue[(Int, Int)] = new Queue[(Int, Int)]
      def mark (queue : Queue[(Int, Int)], x : Int, y : Int, target : Char, label : Char) : Unit = {
        if(x >= 0 && y >= 0 && x < grid.length && y < grid(0).length && grid(x)(y) == target) {
          queue.enqueue((x, y))
          grid(x)(y) = label
        }
      }

      for ( i <- 0 until grid.length ; j <- 0 until grid(0).length if grid(i)(j) == '1') {
        max += 1
        queue.clear()
        var label : Char = '2'
        mark(queue, i, j, '1', label)
        while(!queue.isEmpty) {
          val (x, y) = queue.dequeue()
          mark(queue, x - 1, y, '1', label)
          mark(queue, x, y - 1, '1', label)
          mark(queue, x + 1, y, '1', label)
          mark(queue, x, y + 1, '1', label)
        }
      }
      grid.foreach(row => {row.foreach(printf("%2c ", _)); println()})
      max
    }

    data.foreach(row => {row.foreach(printf("%2c ", _)); println()})
    println()
    println()
    println(numIslands(data))

  }

  /*
    求最大岛屿的面积
    1 为陆，0 为水，被水围绕（或处于矩阵边缘）的陆地即为岛
   */
  @Test
  def maxIslandArea(): Unit = {
    /*
      1  0  0  1  0
      0  1  1  0  0
      0  0  1  0  0
      0  1  0  1  0
      1  0  1  0  1
    */
    val data = Array(Array(1,0,0,1,0),Array(0,1,1,0,0),Array(0,0,1,0,0),Array(0,1,0,1,0),Array(1,0,1,0,1))
    def numIslands(grid: Array[Array[Int]]): Int = {
      if(grid == null || grid.length == 0 || grid(0) == null || grid(0).length == 0) {
        -1
      }
      var max = 0
      import scala.collection.mutable.Queue
      val queue : Queue[(Int, Int)] = new Queue[(Int, Int)]
      def mark (queue : Queue[(Int, Int)], x : Int, y : Int, target : Int, label : Int) : Unit = {
        if(x >= 0 && y >= 0 && x < grid.length && y < grid(0).length && grid(x)(y) == target) {
          queue.enqueue((x, y))
          grid(x)(y) = label
        }
      }

      for ( i <- 0 until grid.length ; j <- 0 until grid(0).length if grid(i)(j) == 1) {
        var area = 0
        queue.clear()
        var label : Char = 2
        mark(queue, i, j, 1, label)
        while(!queue.isEmpty) {
          val (x, y) = queue.dequeue()
          area += 1
          mark(queue, x - 1, y, 1, label)
          mark(queue, x, y - 1, 1, label)
          mark(queue, x + 1, y, 1, label)
          mark(queue, x, y + 1, 1, label)
        }
        if(area > max) max = area
      }
      grid.foreach(row => {row.foreach(printf("%2s ", _)); println()})
      max
    }

    data.foreach(row => {row.foreach(printf("%2s ", _)); println()})
    println()
    println()
    println(numIslands(data))

  }

  /*
    求岛屿的周长 （本题假设只有一个无湖岛）
    1 为陆，0 为水，被水围绕（或处于矩阵边缘）的陆地即为岛
   */
  @Test
  def islandPerimeter = {
    /*
      0  0  1  0  0
      0  1  1  0  0
      0  0  1  0  0
      0  1  1  1  0
      0  0  1  0  0
    */
    var data = Array(Array(0,0,1,0,0),Array(0,1,1,0,0),Array(0,0,1,0,0),Array(0,1,1,1,0),Array(0,0,1,0,0))

    def islandPerimeter(grid: Array[Array[Int]]): Int = {
      if (grid == null || grid.length == 0 || grid(0) == null || grid(0).length == 0) 0

      import scala.collection.mutable.Stack
      val stack : Stack[(Int, Int)] = new Stack[(Int, Int)]
      var perimeter = 0
      def cal(stack : Stack[(Int, Int)], x : Int, y : Int): Unit = {
        if(x < 0 || y < 0 || x >= grid.length || y >= grid(0).length || grid(x)(y) != 1) return

        stack.push((x,y))
        grid(x)(y) = 2
        if(x <= 0 || grid(x-1)(y) == 0) perimeter += 1
        if(y <= 0 || grid(x)(y-1) == 0) perimeter += 1
        if(x >= grid.length - 1 || grid(x+1)(y) == 0) perimeter += 1
        if(y >= grid(0).length - 1 || grid(x)(y+1) == 0) perimeter += 1
      }
      for ( i <- 0 until grid.length ; j <- 0 until grid(0).length ; if grid(i)(j) == 1 ) {
        cal(stack,i, j)
        while(!stack.isEmpty) {
          val (x, y) = stack.pop()
          cal(stack, x - 1, y)
          cal(stack, x, y - 1)
          cal(stack, x + 1, y)
          cal(stack, x, y + 1)
        }

        return perimeter
      }
      perimeter
    }

    println(islandPerimeter(data))
  }

}
