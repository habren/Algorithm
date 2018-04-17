package com.jasongj.algorithm

/**
  * Created by Jason Guo ( jason.guo.vip@gmail.com )
  */
object ArrayDemo extends App{
//  val array = Array(3, 6, 2, 5, 9, 3, 2, 1, 3, 4)
  val array = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 1)

  "abc".charAt(0)

  for ( i <- 0 to array.length - 1) {
    moveData(array, i)
  }

  def moveData (array : Array[Int], index : Int) : Unit = {
    if (array(index) != array(array(index))) {
      val temp = array(array(index))
      array(array(index)) = array(index)
      array(index) =  temp
      moveData(array, index)
    } else if (array(index) != index) {
      println(array(index))
    }

  }

}
