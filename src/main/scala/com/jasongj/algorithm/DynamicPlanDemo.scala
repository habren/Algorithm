package com.jasongj.algorithm

/**
  * Created by Jason Guo ( jason.guo.vip@gmail.com )
  */
object DynamicPlanDemo extends App {

  def cal(n : Int, total : Int) : Int = {
    n match {
      case 2 => 3 + total
      case 1 => 2 + total
      case 0 => 1 + total
      case x: Int => total + cal(x - 1, total) + cal(x - 2, total)

    }
  }

  cal(4, 0)

}

