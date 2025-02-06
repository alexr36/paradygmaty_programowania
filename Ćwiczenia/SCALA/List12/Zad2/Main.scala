package Zad2

import akka.actor.{ActorRef, ActorSystem, Props}

class Main {
  //  == Class fields  =================================================================================================
  
  private val system_name = "BallThrowSystem"
  private val actor_system = ActorSystem(system_name)
  private val players_count = 5
  private val players = new Array[ActorRef](players_count)
  private val max_throws = 10


  //  ==  Private methods  =============================================================================================

  private def startGame(): Unit = {
    generatePlayers()
    val monitor = actor_system.actorOf(Props(new GameMonitor(actor_system, players, players_count)))
    players(0) ! Start(max_throws)
  }


  private def generatePlayers(): Unit = {
    for (i <- 0 to players_count - 1) {
      players(i) = actor_system.actorOf(Props(new Player(i + 1, players, max_throws)))
    }
  }
}


//  Companion object
object Main extends App {
  val main = new Main()
  main.startGame()
}
