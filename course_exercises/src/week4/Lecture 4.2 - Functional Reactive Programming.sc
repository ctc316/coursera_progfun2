import week4._

class BankAccount {
  val balance = Var(0)
  def deposit(amount: Int): Unit = {
    if (amount > 0){
      val b = balance()
      balance() = b + amount
    }
  }
  def withdraw(amount: Int): Unit = {
    if( 0< amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("insufficient funds")
  }
}

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a, b))
c()
a deposit 20
c()
b deposit 30
c()
val xchange = Signal(246.00)
val inDollar = Signal(c() * xchange())
inDollar()
b withdraw 10
inDollar()