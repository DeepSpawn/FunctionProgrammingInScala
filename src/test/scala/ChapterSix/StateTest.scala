package ChapterSix

import ChapterSix.Machine
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by gtaylor on 22/05/2016.
  */
class StateTest  extends FlatSpec with Matchers  {

  "StateMachine" should "return (14 coins, 1 candy) when starting at (10 coins, 5 candies) and 4 candies are bought" in {
    State.simulateMachine(List(Coin,Turn,Coin,Turn,Coin,Turn,Coin,Turn)).run(Machine(true,5,10)) shouldEqual ((1,14), Machine(true,1,14))
  }

  "StateMachine" should "return (11 coins, 4 candy) when starting at (10 coins, 5 candies) and 1 candies are bought" in {
    State.simulateMachine(List(Coin,Turn)).run(Machine(true,5,10)) shouldEqual ((4,11), Machine(true,4,11))
  }


  "StateMachine" should "return (1, 1) when starting at (1,1) should result in 1 candies  bought" in {
    State.simulateMachine(List(Coin,Turn)).run(Machine(true,1,1)) shouldEqual ((0,2),Machine(true,0,2))
  }


//  The method simulateMachine should operate the machine based on the list of inputs and
//  return the number of coins and candies left in the machine at the end. For exam- ple,
//  if the input Machine has 10 coins and 5 candies,
//  and a total of 4 candies are suc- cessfully bought,
//  the output should be (14, 1).

}
