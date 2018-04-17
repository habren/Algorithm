package com.jasongj.algorithm

/**
  * Created by Jason Guo
  */
object TypeParameterDemo extends App {

  val person = new Person("person-first", "person-second")
  println(person.firstName)

  val student = new Student("student-first", "student-second", "student-id")
  println(student.firstName)

  val master = new Master("master", "master-first", "master-second", "master-id")
  println(master.degree)

  def makeFriendWith(s: Student, f: Friend[Student]): Unit = {
    f.befriend(s)
  }

  makeFriendWith(master, new PersonFriend())


  new TechBook().setId("book id").setCategory("tech book category")

}

class Pair[+T](val first: T, val second: T)

class Person(val firstName : String, val secondName : String)

class Student (override val firstName : String, override val secondName : String, val id : String) extends Person (firstName, secondName)

class Master (val degree : String, override val firstName : String, override val secondName : String, override val id : String) extends Student(firstName, secondName, id)

trait Friend[-T] {
  def befriend(someone : T)
}

class PersonFriend extends Friend[Person] {
  override def befriend(someone: Person): Unit = {
    println(s"New Person friend: $someone")
  }
}

class StudentFriend extends Friend[Student] {
  override def befriend(someone: Student): Unit = {
    println(s"New Student friend : $someone")
  }
}

class MasterFriend extends Friend[Master] {
  override def befriend(someone: Master): Unit = {
    println(s"New Master friend : $someone")
  }
}

class Book {
  def setTitle(title : String) : this.type = {
    println(s"title: $title")
    this
  }

  def setId(id : String) : this.type =  {
    println(s"id: $id")
    this
  }
}

class TechBook extends Book {
  def setCategory(category : String) : this.type = {
    println(s"category: $category")
    this
  }
}