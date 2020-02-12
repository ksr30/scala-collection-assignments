package com.knoldus

import scala.collection.immutable.ListMap

class Ops {

  case class Student(id: Long, name: String)

  case class Marks(subjectId: Int, studentId: Long, marksObtained: Float)

  val nameList = List(Student(1, "Yuri"),
    Student(2, "Todd"),
    Student(3, "Tyrone"),
    Student(4, "Aaron"),
    Student(5, "Ferdinand"),
    Student(6, "Zeph"),
    Student(7, "Karleigh"),
    Student(8, "Jonah"),
    Student(9, "Gil"),
    Student(10, "Kerry"))


  val marksList = List(
    Marks(1, 1, 78),
    Marks(2, 1, 50),
    Marks(1, 2, 95),
    Marks(2, 2, 68),
    Marks(1, 3, 85),
    Marks(2, 3, 82),
    Marks(1, 4, 91),
    Marks(2, 4, 86),
    Marks(1, 5, 60),
    Marks(2, 5, 49),
    Marks(1, 6, 43),
    Marks(2, 6, 37),
    Marks(1, 7, 18),
    Marks(2, 7, 82),
    Marks(1, 8, 82),
    Marks(2, 8, 58),
    Marks(1, 9, 73),
    Marks(2, 9, 50),
    Marks(1, 10, 72),
    Marks(2, 10, 36),
  )
  /*1)
  Input:- (subjectId, percentage, pass/fail)
  Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
  for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
  Note:- percentage is the input which defines the minimum passing criteria
    e.g.
      Pass count: 15
  Fail count: 10*/

  def passOrFail(subjectId: Int, percentage: Int, status: String): Int = {
    if (status.equals("pass")) {
      val tempList = for {s <- nameList
                          m <- marksList
                          if (s.id == m.studentId) && (m.marksObtained > percentage) && (m.subjectId == subjectId)
                          } yield {
        s
      }
      tempList.length
    }
    else {
      val tempList = for {s <- nameList
                          m <- marksList
                          if (s.id == m.studentId) && (m.marksObtained < percentage) && (m.subjectId == subjectId)
                          } yield {
        s
      }
      tempList.length

    }


  }

  /*2)
  Input:- (subjectId, count, top/bottom)
  Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId
  e.g.
    input: 1 5 top
    output:
    Kunal 85
  Himanshu 84
  Geetika 83
  Anmol 82
  Mahesh 81*/

  def maxOrMin(subjectId: Int, count: Int, topOrBottom: String): Map[String, Float] = {
    val map = for {
      s <- nameList
      m <- marksList
      if m.subjectId == subjectId && s.id == m.studentId
    } yield {
      s.name -> m.marksObtained
    }
    if (topOrBottom != "top") {
      val res = ListMap(map.sortBy(_._2): _*)
      res.take(count)
    }
    else {
      val res = ListMap(map.sortWith(_._2 > _._2): _*)
      res.take(count)
    }
  }

  /*3)
  Input:-
    (top/bottom, count)
  OutPut:-
    Overall top/least scorer based on all the subjects score, fetch students name
  count- input defines that how much students name are to be printed on console
  e.g.
    input: top 2

  output:
    Himanshu 75%
    Geetika 74%*/
  def f1(student: Student): Float = {
    marksList.filter(_.studentId == student.id).map(_.marksObtained).sum / 2
  }

  def scorer(topBo: String, count: Int) = {

    val tempL = nameList.map {
      case x => x.name -> f1(x)
    }
    if (topBo != "top") ListMap(tempL.toSeq.sortBy(_._2): _*).toMap.take(count) else ListMap(tempL.toSeq.sortWith(_._2 > _._2): _*).toMap.take(count)

  }

  val c = scorer("top", 3)
  /*4)
  Input:-
    (percentage, good_scholarship, normal_or_no_scholarship)
  Output:- two groups of students with the amount of scholarship
    e.g.
      input: 85% 2000 500
  output:
    Kunal 2000
  Himanshu 500
  Geetika 2000
  Mahesh 500*/

  def scolarshipCal(percenatge: Float, scolarGrp: Int, nonGrp: Int = 0) = {
    val tempL = nameList.map {
      case x => x.name -> f1(x)
    }.toMap

    tempL.map {
      case (x, y) => if (y > percenatge) x -> scolarGrp else x -> nonGrp
    }

  }

  //Input:-
  //  (pass/fail, percentage)
  //count and print the number of students and all names who are passed/fail,
  //Pass or fail would be decided by percentage input field.
  //e.g.
  //  input: fail 30
  //output:
  //  Kunal 28%
  //  Himanshu 29%

  def passOrFail(passOrFail: String, percentage: Int) = {
    val tempL = nameList.map {
      case x => x.name -> f1(x)
    }
    if (passOrFail == "pass")
      tempL.filter(x => x._2 > percentage)


    else tempL.filter(x => x._2 < percentage)

  }

  //6) Find the student(s) who have scored 95% or above and print its details.
  //input: 95%
  //  output:
  //Kunal 95%
  //  Himanshu 96%
  //  Geetika 97%

  def overPercentage() = {
    val tempL = nameList.map {
      case x => x.name -> f1(x)
    }
    tempL.filter(x => x._2 > 95)
  }


}

object Ops extends App {
  val op = new Ops
}