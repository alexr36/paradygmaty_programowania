package Zad1

import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props

class Main {
  //  == Class fields  =================================================================================================

  private val system_name = "PingPongSystem"
  private val actor_system = ActorSystem(system_name)
  private val max_hits = 3
  private var player_1: ActorRef = _
  private var player_2: ActorRef = _
  private var monitor: ActorRef = _


  //  ==  Private methods  =============================================================================================

  private def initializeActors(): Unit = {
    player_1 = actor_system.actorOf(Props(new Player("Player 1")))
    player_2 = actor_system.actorOf(Props(new Player("Player 2")))

    setOpps()

    monitor = actor_system.actorOf(Props(new GameMonitor(player_1, player_2, actor_system)))
  }


  private def setOpps(): Unit = {
    player_1 ! SetOpp(player_2)
    player_2 ! SetOpp(player_1)
  }


  private def startProgram(): Unit = {
    initializeActors()
    player_1 ! Start(max_hits)
  }
}


//  Companion object
object Main extends App {
  private val main = new Main
  main.startProgram()
}
