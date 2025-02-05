package Zad1

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}


class GameMonitor(private val player_1: ActorRef, private val player_2: ActorRef, private val system: ActorSystem) extends Actor {
  private var terminated_actors = 0

  context.watch(player_1)
  context.watch(player_2)

  
  override def receive: Receive = {
    case Terminated(ref) =>
      terminated_actors += 1

      if (terminated_actors == 2) {
        println("Both players finished the game. Closing the program...")
        system.terminate()
      }
  }
}
