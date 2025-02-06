package Zad1

import akka.actor.Actor
import akka.actor.ActorRef


//  Actor messages
case class SetOpp(ref: ActorRef)
case class Start(max_hits: Int)
case object Stop
case object Ping
case object Pong


class Player(private val name: String) extends Actor {
  //  == Class fields  =================================================================================================
  
  private val ping_msg = "*ping*"
  private val pong_msg = "*pong*"
  private val hit_time = 1000   //  1s
  private var hit_counter = Int.MaxValue
  private var opponent: ActorRef = _


  //  == Public methods  ===============================================================================================
  
  override def receive: Receive = {
    case SetOpp(ref) => setOpponent(ref)
    case Start(max_hits) => startGame(max_hits)
    case Stop => finishGame()
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
      Thread.sleep(hit_time)
      hit_counter -= 1

      if (msg == ping_msg) opponent ! Pong
      else if (msg == pong_msg) opponent ! Ping
    }
    else finishGame()
  }


  private def startGame(max_hits: Int): Unit = {
    hit_counter = max_hits
    println(s"$name is ready to play!")
    self ! Ping
  }


  private def finishGame(): Unit = {
    println(s"$name stopped playing.")
    opponent ! Stop
    context.stop(self)
  }
}
