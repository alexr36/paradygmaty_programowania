package Zad2

import akka.actor.{Actor, ActorRef}
import scala.util.Random


//  Actor messages
case class Start(max_throws: Int)
case class Ball(count: Int)
case object Stop


class Player(val num: Int, val players: Array[ActorRef], val max_throws: Int) extends Actor {
  //  == Class fields  =================================================================================================

  private val rand: Random = new Random()
  private val players_count = players.length
  private val throw_time = 1000   //  1s


  //  == Public methods  ===============================================================================================

  override def receive: Receive = {
    case Start(max_throws) => startGame(max_throws)
    case Stop => stopPlaying()
    case Ball(count) => handlePlay(count + 1)
  }


  //  ==  Private methods  =============================================================================================

  //  Starting and finishing the game methods
  private def startGame(max_throws: Int): Unit = {
    self ! Ball(0)
  }


  private def finishGame(): Unit = {
    players.foreach(player => player ! Stop)
  }


  private def stopPlaying(): Unit = {
    println(s"Player $num stopped playing.")
    context.stop(self)
  }


  //  Play-handling methods
  private def handlePlay(throw_num: Int): Unit = {
    if (max_throws < throw_num) finishGame()
    else handleBallThrow(throw_num)
  }


  private def handleBallThrow(throw_num: Int): Unit = {
    val next_player_num = chooseAnotherPlayerNum()

    println(s"Throw $throw_num: Player $num throws the ball to Player $next_player_num.")
    Thread.sleep(throw_time)
    players(next_player_num - 1) ! Ball(throw_num)
  }


  //  Other aux methods
  private def chooseAnotherPlayerNum(): Int = {
    var destination_num = rand.nextInt(players_count) + 1

    //  Player cannot throw the ball to itself
    while (destination_num == num) {
      destination_num = rand.nextInt(players_count) + 1
    }

    destination_num
  }
}


