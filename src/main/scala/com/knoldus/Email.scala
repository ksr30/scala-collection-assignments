//Write regular expression for email parsing.
//Description :
//
//val EMAIL = “ regular expression definition”.r
//val EMAIL(user, domain) = “knol@knoldus.com”
//
//user = knol
//domain = knoldus.com

object Email {
  def email(mail:String): String ={
    val email = """(^[A-Za-z0-9][A-Za-z0-9]*)@((?:[A-Za-z0-9]+\.)+[A-Za-z]{2,63}$)""".r
    mail match{
      case email(user,domain) => s"user =$user\n domain =$domain"
      case  _ => "invalid"
    }
  }
  email("krishna.singh@gmail.com")
}