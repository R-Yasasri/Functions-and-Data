package functions_and_data

object bank extends App {

  var bank: List[Account] = List()

  class Account(id: String, n: Int, b: Double) {
    val nic: String = id
    val acnumber: Int = n
    var balance: Double = b

    override def toString =
      "[" + nic + ":" + acnumber + ":" + balance + "]"

    def depositInterest(interest: Double) = {
      this.balance = this.balance + this.balance * interest;
    }

    def overdraftInterest(interest: Double) = {
      this.balance = this.balance - this.balance * interest;
    }
  }

  val negativeFilter = (account: Account) => {
    account.balance < 0
  }

  val overdraft = (b: List[Account]) => b.filter(negativeFilter);

  val balance = (b: List[Account]) => b.reduce((acc0, acc1) => new Account("123", 123, acc0.balance + acc1.balance));

  val m = (acc: Account) => {
    acc.balance > 0 match {
      case true => acc.depositInterest(0.5);
      case false => acc.overdraftInterest(0.1);
    };

  }

  val interest = (b: List[Account]) => b.map(m)

  def example() = {
    bank = bank :+ new Account("123", 123, 12.5);
    println("account created: nic=123 acnumber=123 balance=12.5");
    bank = bank :+ new Account("456", 456, -0.56);
    println("account created: nic=456 acnumber=456 balance=-0.56");

    println("Overdraft account is= " + overdraft(bank)(0).nic + " with balance " + overdraft(bank)(0).balance);
    println("sum of all account balances is " + balance(bank).balance);
    interest(bank);

    println("new balance of account 123 is: " + bank(0).balance);
    println("new balance of account 456 is: " + bank(1).balance);

  }

  example();
}







