package com.jasongj.algorithm.core;

import java.io.File

import scala.beans.BeanProperty
import scala.reflect.ClassTag

/**
 * Created by Jason Guo (jason.guo.vip@gmail.com)
 */
class ScalaDemo {
//  @BeanProperty
  var name = "Mike"
}

object ScalaDemo {

    def main(args: Array[String]): Unit = {
      def makeArray[T : ClassTag](elements: T*) = Array[T](elements: _*)
      makeArray(1,2,3,4).foreach(println)
      val demo = new ScalaDemo()
      demo.name = "hello"
      println(demo.name)
      import sys.process._
//      "ls /Users/juguo/Desktop" !
      val p = Process("echo hello", new File("/Users/juguo/Desktop"))
      "ls -lh ." #| p !
      val test = "([0-9]+) ([a-z]+)".r
      val test(num, item) = "99 bottles"
      println(num)
      println(item)
      val a = 1::2::Nil
      println(a)

      def numsFrom(n: BigInt) : Stream[BigInt] = n #:: numsFrom(n + 1)

      val test1 = new ScalaDemo() with ConsoleLogger with TimestampLogger with ShortLogger
      test1.log("test")
    }

}

trait Logger {
  def log(msg: String)
}


trait Logged extends Logger{
  override def log(msg: String) {}
}

trait ConsoleLogger extends Logged{
  override def log(msg: String): Unit = {
    println("ConsoleLogger:" + msg)
  }
}


trait TimestampLogger extends Logged {
  override def log(msg: String): Unit = {
    super.log("TimestampLogger:" + msg)
  }
}

trait ShortLogger extends Logged {
  override def log(msg: String): Unit = {
    super.log("ShortLogger:" + msg)
  }
}


