package Zad2

import akka.actor.{Actor, ActorRef, ActorSystem, Terminated}

class GameMonitor(private val system: ActorSystem, private val players: Array[ActorRef], private val players_count: Int) extends Actor {
  private var terminated_actors = 0
  
  //  Watch every player
  players.foreach(player => context.watch(player))
  
  
  override def receive: Receive = {
    case Terminated(ref) => {
      terminated_actors += 1

      if (terminated_actors == players_count) {
        println("All players finished the game. Closing the program...")
        system.terminate()
      }
    }
  }
}
