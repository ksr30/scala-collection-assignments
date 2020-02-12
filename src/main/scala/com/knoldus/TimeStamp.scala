//1. Get the day out of a timestamp value with format: “dd-mm-yyyy HH:MM:SSS”.
object TimeStamp extends App {
  def apply(dd: Int, mm: Int, yyyy: Int, HH: Int, MM: Int, SSS: Int): String = {
    dd + "-" + mm + "-" + yyyy + " " + HH + ":" + MM + ":" + "SSS"
  }

  def unapply(dateTime:String): Option[String] ={
    val day = dateTime split " "
    if(day.length == 2) {
      val day1 = day(0) split "-"
      Some(day1(0))
    } else None
  }
}



