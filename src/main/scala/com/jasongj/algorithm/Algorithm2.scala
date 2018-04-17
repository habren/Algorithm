package com.jasongj.algorithm

import org.testng.annotations.Test

/**
  * Created by Jason Guo ( jason.guo.vip@gmail.com )
  */
class Algorithm2 {

  /*
  合并两个有序列表
   */
  @Test
  def mergeSortedLinkedlist = {
    def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
      if (l1 == null) {
        l2
      } else if (l2 == null) l1
      else {
        var h1: ListNode = l1
        var h2: ListNode = l2
        val head = if (l1._x < l2._x) {
          h1 = l1.next;
          l1
        } else {
          h2 = l2.next;
          l2
        }
        var tail: ListNode = head
        import scala.util.control.Breaks._
        breakable {
          while (tail != null) {
            if (h1 == null) {
              tail.next = h2
              break
            } else if (h2 == null) {
              tail.next = h1
              break
            } else if (h1._x < h2._x) {
              tail.next = h1
              tail = h1
              h1 = h1.next
            } else {
              tail.next = h2
              tail = h2
              h2 = h2.next
            }
          }
        }
        head
      }
    }

    case class ListNode(val _x: Int, var next: ListNode)
    val l1: ListNode = ListNode(1, ListNode(2, ListNode(4, null)))
    val l2: ListNode = ListNode(1, ListNode(3, ListNode(4, null)))

    var result = mergeTwoLists(l1, l2)
    while (result != null) {
      println(result._x)
      result = result.next
    }

  }

  /*
  链表反转
   */
  @Test
  def reverseLinkedList: Unit = {
    case class ListNode(val _x: Int, var next: ListNode)
    val list: ListNode = ListNode(1, ListNode(2, ListNode(4, ListNode(3, ListNode(5, ListNode(8, ListNode(7, ListNode(6, null))))))))

    def reverse1(head: ListNode) = {
      var newHeader: ListNode = null
      var current: ListNode = list
      var next: ListNode = null

      while (current != null) {
        next = current.next
        current.next = newHeader
        newHeader = current
        current = next
      }
      println(newHeader)
    }

    def reverse2(head: ListNode): Unit = {
      var newHeader: ListNode = head
      var current: ListNode = head.next
      var next: ListNode = null

      while (current != null) {
        next = current.next
        current.next = newHeader
        newHeader = current
        current = next
      }
      println(newHeader)
    }

    println(list)

    println()

    reverse1(list)

    println()

    reverse2(list)

  }

  /*
  快排  递归解法
   */
  @Test
  def quickSortRecur = {
    val data = Array(1, 8, 7, 6, 3, 5, 7, 2, 4, 10, 9, 0)

    import scala.util.control.Breaks._

    def partition(array: Array[Int], begin: Int, end: Int): Unit = {
      if (begin >= end) return
      var i = begin
      var j = end

      def swap(x: Int, y: Int) = {
        val tmp = array(x)
        array(x) = array(y)
        array(y) = tmp
      }

      val mid = begin >> 1 + end >> 1
      if (mid > begin) {
        if (array(begin) > array(end)) swap(begin, end)
        if (array(mid) > array(end)) swap(mid, end)
        if (array(mid) > array(begin)) swap(mid, begin)
      }

      breakable(
        while (i < j) {
          val ref = array(i)
          breakable(
            while (i < j && array(j) >= array(i)) {
              j -= 1
            }
          )
          array(i) = array(j)
          array(j) = ref

          breakable(
            while (i < j && array(i) <= array(j)) {
              i += 1
            }
          )
          array(j) = array(i)
          array(i) = ref
        }
      )

      partition(array, begin, i - 1)
      partition(array, i + 1, end)
    }

    partition(data, 0, data.length - 1)

    data.foreach(printf("%3s", _))
    println()

  }

  /*
  快排 非递归解法
   */
  @Test
  def quickSortNonRecur = {
    import scala.util.control.Breaks._

    def partition(array: Array[Int], begin: Int, end: Int): Int = {
      if (begin >= end) -10
      var i = begin
      var j = end

      def swap(x: Int, y: Int) = {
        val tmp = array(x)
        array(x) = array(y)
        array(y) = tmp
      }

      val mid = begin >> 1 + end >> 1
      if (mid > begin) {
        if (array(begin) > array(end)) swap(begin, end)
        if (array(mid) > array(end)) swap(mid, end)
        if (array(mid) > array(begin)) swap(mid, begin)
      }

      breakable(
        while (i < j) {
          val ref = array(i)
          breakable(
            while (i < j && array(j) >= array(i)) {
              j -= 1
            }
          )
          array(i) = array(j)
          array(j) = ref

          breakable(
            while (i < j && array(i) <= array(j)) {
              i += 1
            }
          )
          array(j) = array(i)
          array(i) = ref
        }
      )
      i
    }

    import scala.collection.mutable.Stack
    val stack: Stack[(Int, Int)] = new Stack[(Int, Int)]


    val data = Array(1, 8, 7, 6, 3, 5, 7, 2, 4, 10, 9, 0)
    stack.push((0, data.length - 1))

    while (!stack.isEmpty) {
      val (begin, end) = stack.pop()
      val mid = partition(data, begin, end)
      if (mid > begin + 1) stack.push((begin, mid - 1))
      if (mid < end - 1) stack.push((mid + 1, end))
    }

    data.foreach(printf("%3s", _))
    println()


  }



  /*
* 链表排序 时间复杂度为O(n*logn)，空间复杂主度为O(1)
* 快排分治法
* */
  @Test
  def linkListSort = {
    case class ListNode(var x: Int, var next: ListNode)
    def sortList(head: ListNode): ListNode = {
      def partition(head: ListNode): (ListNode, ListNode) = {
        if (head == null || head.next == null) (head, head)
        else {
          var lowHead: ListNode = null
          var lowTail: ListNode = null
          var highHead: ListNode = null
          var highTail: ListNode = null
          var midHead: ListNode = head
          var midTail: ListNode = head

          val ref: Int = head.x
          var index = head.next
          midTail.next = null
          while (index != null) {
            val tmp = index.next
            index.next = null
            if (index.x < ref) {
              index.next = null
              if (lowHead == null) {
                lowHead = index
              } else {
                lowTail.next = index
              }
              lowTail = index
            } else if (index.x > ref) {
              if (highHead == null) {
                highHead = index
              } else {
                highTail.next = index
              }
              highTail = index
            } else {
              midTail.next = index
              midTail = index
            }
            index = tmp
          }
          val (lh, lt) = partition(lowHead)
          val (hh, ht) = partition(highHead)
          //          midTail.next = hh
          if (lh == null) {
            if (hh == null) (midHead, midTail)
            else {
              midTail.next = hh
              (midHead, ht)
            }
          } else {
            lt.next = midHead
            if (hh == null) (lh, midTail)
            else {
              midTail.next = hh
              (lh, ht)
            }
          }

        }
      }

      partition(head)._1
    }

    val data: ListNode = ListNode(1, ListNode(2, ListNode(4, ListNode(3, ListNode(5, ListNode(8, ListNode(7, ListNode(6, null))))))))
    println(sortList(data))
  }

  /*
  * 链表排序 时间复杂度为O(n*logn)，空间复杂主度为O(1)
  * 快排解法
  * */
  @Test
  def listSort = {
    case class ListNode(var x: Int, var next: ListNode)
    def sortList(head: ListNode): ListNode = {
      def partition(head: ListNode, tail : ListNode) : ListNode = {
        if(head == null || head == tail) return head
        var lastSmall : ListNode = head
        var current : ListNode = head.next
        val ref = head.x
        while(current != null) {
          if(current.x < ref) {
            lastSmall = lastSmall.next
            val tmp = lastSmall.x
            lastSmall.x = current.x
            current.x = tmp
          }
          current = current.next
        }
        head.x = lastSmall.x
        lastSmall.x = ref

        partition(head, lastSmall)
        partition(lastSmall.next, tail)
        head
      }
      var tail = head
      if(head == null) head
      while(tail.next != null) tail = tail.next
      partition(head, tail)
      head
    }
    val data: ListNode = ListNode(1, ListNode(2, ListNode(4, ListNode(3, ListNode(5, ListNode(8, ListNode(7, ListNode(6, null))))))))
    println(sortList(data))

  }

  /*
  链表直接插入排序
  */
  @Test
  def insertSortLinkedList = {
    def insertionSortList(head: ListNode): ListNode = {
      if(head == null || head.next == null) return head
      var header = head
      var index = head.next
      var tail = head
      tail.next = null
      while(index != null) {
        val tmp = index.next
        if (header.x > index.x) {
          index.next = header
          header = index
        } else if (index.x > tail.x) {
          tail.next = index
          tail = index
          tail.next = null
        } else {
          var cur = header
          import scala.util.control.Breaks._
          breakable {
            while(cur !=  null) {
              if(cur.next == null) {
                cur.next = index
                tail = index
                index.next = null
                break
              } else if(cur.x <= index.x && cur.next.x > index.x) {
                val next = cur.next
                cur.next = index
                index.next = next
                break
              }
//              tail.next = null
              cur = cur.next
            }
          }
        }
        index = tmp
      }
      header
    }

    case class ListNode(var x: Int, var next: ListNode)
//    val data: ListNode = ListNode(2, ListNode(1, null))
    val data: ListNode = ListNode(1, ListNode(2, ListNode(4, ListNode(3, ListNode(5, ListNode(8, ListNode(7, ListNode(6, null))))))))
    println(insertionSortList(data))
  }

  /*
    交换链表相邻节点
    https://leetcode-cn.com/problems/swap-nodes-in-pairs/description/

    给定一个链表，对每两个相邻的结点作交换并返回头节点。
    例如：
      给定 1->2->3->4，你应该返回 2->1->4->3。

    你的算法应该只使用额外的常数空间。不要修改列表中的值，只有节点本身可以​​更改。
   */

  @Test
  def swapNodeOfList = {
    def swapPairs(head: ListNode): ListNode = {
      if(head == null || head.next == null) {
        return head
      }
      var header = head.next
      val next = header.next
      header.next = head
//      head.next = next

      head.next = swapPairs(next)
      header
    }
    val data: ListNode = ListNode(1, ListNode(2, ListNode(4, ListNode(3, ListNode(5, ListNode(8, ListNode(7, ListNode(6, null))))))))
    println(swapPairs(data))

  }

  /*
  插入合并区间
  https://leetcode-cn.com/problems/insert-interval/description/
  给出一个无重叠的按照区间起始端点排序的区间列表。

  在列表中插入一个新的区间，你要确保列表中的区间仍然有序且不重叠（如果有必要的话，可以合并区间）。

  示例 1:
  给定区间 [1,3],[6,9]，插入并合并 [2,5] 得到 [1,5],[6,9].

  示例 2:
  给定区间 [1,2],[3,5],[6,7],[8,10],[12,16]，插入并合并 [4,9] 得到 [1,2],[3,10],[12,16].

  这是因为新的区间 [4,9] 与 [3,5],[6,7],[8,10] 重叠。
   */
  @Test
  def insertRange = {
    def insert(intervals: List[Interval], newInterval: Interval): List[Interval] = {
      if(intervals == null || intervals.length == 0) return List[Interval](newInterval)
      var result : List[Interval] = List[Interval]()
      var start : Integer = newInterval.start
      var end : Integer = newInterval.end

      import java.lang.Math.{min, max}
      for ( i <- 0 until intervals.length ) {
        val interval = intervals(i)
        if (start == null) {
          result = result :+ interval
        } else {
          if (interval.end < start) {
            result = result :+ interval
          } else if (interval.start >= start && interval.end <= end) {
          } else if (interval.start <= start && interval.end >= end) {
            start = interval.start
            end = interval.end
          } else if (interval.start >= start && interval.start <= end) {
            end = max(end, interval.end)
          } else if (interval.end >= start && interval.end <= end) {
            start = min(start, interval.start)
          } else if (interval.start > end) {
            result = result :+ Interval(start, end)
            result = result :+ interval
            start = null
            end = null
          }
        }
      }
      if(start != null) result = result :+ Interval(start, end)
      result
    }

    val data = Interval(1,5) :: Nil
    var target = Interval(5, 7)
//    val data = Interval(2,5) :: Interval(6,7) :: Interval(8, 9) :: Nil
//    val target = Interval(0,1)
    println(insert(data, target))

  }

  /*
    合并区间
    给出一个区间的集合, 请合并所有重叠的区间。

    示例：
    给出 [1,3],[2,6],[8,10],[15,18],
    返回 [1,6],[8,10],[15,18].
   */

  def mergeRange = {
    def merge(intervals: List[Interval]): List[Interval] = {

      null
    }
  }

  @Test
  def addLinkedList = {
    case class ListNode(var _x : Int = 0) {
      var x : Int = _x
      var next : ListNode = null
      def this(_x : Int, _next : ListNode) {
        this(_x)
        next = _next
      }
    }
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
      if(l1 == null) return l2
      if(l2 == null) return l1
      var sum = l1.x + l2.x
      var head = ListNode(sum)
      if(sum >= 10) {
        head.x = sum - 10
        head.next = ListNode(1)
      } else {
        head.x = sum
        head.next = ListNode(0)
      }
      var index1 = l1
      var index2 = l2
      var index = head
      while(index1.next != null || index2.next != null) {
        sum = 0
        if(index1.next != null) {
          index1 = index1.next
          sum += index1.x
        }
        if(index2.next != null) {
          index2 = index2.next
          sum += index2.x
        }
        index = index.next
        sum += index.x
        if(sum >= 10) {
          index.x = sum - 10
          index.next = ListNode(1)
        } else {
          index.x = sum
          index.next = ListNode(0)
        }
      }
      if(index.next.x == 0) {
        index.next = null
      }
      head
    }
    val h1 = new ListNode(2, new ListNode(4, new ListNode(3)))
    val h2 = new ListNode(5, new ListNode(6, new ListNode(4)))
    var result = addTwoNumbers(h1, h2)
    while(result != null) {
      printf("%3s", result.x)
      result = result.next
    }
    println()
  }

  /*
    回文链表

    请检查一个链表是否为回文链表。

    进阶：
    你能在 O(n) 的时间和 O(1) 的额外空间中做到吗？
   */
  @Test
  def palindromeLinkedList = {
    def isPalindrome(head: ListNode): Boolean = {
      if(head == null || head.next == null) true
      else if(head.next.next == null) head.x == head.next.x
      else {
        var fast : ListNode = head
        var slow : ListNode = head
        var center1 : ListNode = null
        var center2 : ListNode = null
        var newHeader : ListNode = null
        var cur : ListNode = head
        while(fast.next != null && fast.next.next != null) {
          fast = fast.next.next
          slow = slow.next
          cur.next = newHeader
          newHeader = cur
          cur = slow
        }
        if(fast.next == null) {
          center2 = new ListNode(slow.x)
          center2.next = slow.next
        } else {
          // fast.next.next == null
          center2 = slow.next
        }
        center1 = new ListNode(slow.x)
        center1.next = newHeader
        while(center1 != null) {
          if(center1.x != center2.x) {
            return false
          }
          center1 = center1.next
          center2 = center2.next
        }
        true
      }
    }

//    val data = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(3, ListNode(2, ListNode(1, null)))))))
//    val data = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(4, ListNode(3, ListNode(2, ListNode(1, null))))))))
    val data = ListNode(1, ListNode(0, ListNode(0, null)))
    println(isPalindrome(data))
  }

  /*
  给定一个非负整数 num。 对于范围 0 ≤ i ≤ num 中的每个数字 i ，计算其二进制数中的1的数目并将它们作为数组返回。

  示例：
  比如给定 num = 5 ，应该返回 [0,1,1,2,1,2].

  进阶：

  给出时间复杂度为O(n * sizeof(integer)) 的解答非常容易。 但是你可以在线性时间O(n)内用一次遍历做到吗？
  要求算法的空间复杂度为O(n)。
  你能进一步完善解法吗？ 在c ++或任何其他语言中不使用任何内置函数（如c++里的 __builtin_popcount）来执行此操作
   */
  @Test
  def count1InBit = {
//    def countBits(num: Int): Array[Int] = {
//      val result = new Array[Int](num+1)
//      result(0) = 0
//      if(num == 0) return result
//      result(1) = 1
//      var i = 1
//      while(i <= (num >> 1)) {
//        if( (i & 1) == 1 ) {
//          result(i) = result(i>>1) + 1
//        } else {
//          result(i) = result(i>>1)
//        }
//        result(i << 1) = result(i)
//        if((i << 1) < num) {
//          result((i << 1)+1) = result(i) + 1
//        }
//        i += 1
//      }
//      result
//    }
    def countBits(num: Int): Array[Int] = {
      val result = new Array[Int](num+1)
      result(0) = 0
      var i = 1
      while(i <= num) {
        result(i) = result(i & i - 1) + 1
        i += 1
      }
      result
    }

    countBits(2).foreach(println(_))
  }


  @Test
  def test = {
    println(2>>1)
  }

}

case class ListNode(var x: Int, var next: ListNode) {
  def this (x : Int) {
    this(x, null)
  }
}
case class Interval(var start : Int = 0, var end : Int = 0)

