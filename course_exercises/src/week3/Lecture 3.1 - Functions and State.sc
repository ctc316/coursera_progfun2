class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int): Int = {
    if( 0< amount && amount <= balance) {
      balance -= amount
      balance
    } else throw new Error("insufficient funds")
  }
}

val acct = new BankAccount
acct deposit(50)
acct withdraw(20)
acct withdraw(20)
acct withdraw(20)