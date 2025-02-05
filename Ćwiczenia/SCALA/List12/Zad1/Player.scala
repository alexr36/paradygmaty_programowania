package Zad1

import akka.actor.Actor
import akka.actor.ActorRef


//  Actor messages
case class SetOpp(ref: ActorRef)
case class Start(max_hits: Int)
case class StopGame()
case object Ping
case object Pong


class Player(private val name: String) extends Actor {
  //  == Class fields  =================================================================================================
  
  private val ping_msg = "*ping*"
  private val pong_msg = "*pong*"
  private val HIT_TIME = 1000   //  1s
  private var hit_counter = Int.MaxValue
  private var opponent: ActorRef = _


  //  == Public methods  ===============================================================================================
  
  override def receive: Receive = {
    case SetOpp(ref) => setOpponent(ref)
    case Start(maxHits) => startGame(maxHits)
    case StopGame => finishGame()
    case Ping => handlePlayer(ping_msg)
    case Pong => handlePlayer(pong_msg)
  }

  
  def setOpponent(opp: ActorRef): Unit = {
    opponent = opp
  }
  

  //  ==  Private methods  =============================================================================================
  
  private def handlePlayer(msg: String): Unit = {
    if (hit_counter > 0) {
      println(msg)
      Thread.sleep(HIT_TIME)
      hit_counter -= 1

      if (msg == ping_msg) opponent ! Pong
      else if (msg == pong_msg) opponent ! Ping
    }
    else finishGame()
  }


  private def startGame(maxHits: Int): Unit = {
    hit_counter = maxHits
    println(s"$name is ready to play!")
    self ! Ping
  }


  private def finishGame(): Unit = {
    println(s"$name stopped playing.")
    opponent ! StopGame
    context.stop(self)
  }
}
