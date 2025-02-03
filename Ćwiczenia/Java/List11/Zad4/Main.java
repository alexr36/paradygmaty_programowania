package List11.Zad4;

import java.util.concurrent.Semaphore;

public class Main {
    //  ==  Class fields  ==============================================================================================

    //  Constants
    private static final int PHILOSOPHERS_NUMBER = 5;
    private static final int MAX_PHILOSOPHERS_IN_DINING_ROOM = 4;
    private static final int PROGRAM_EXECUTION_TIME_QUANTS = 10;

    //  Variables
    private static Philosopher[] philosophers;
    private static Semaphore[] sticks;
    private static Semaphore porter;

    protected static int program_counter = PROGRAM_EXECUTION_TIME_QUANTS;


    //  ==  Public methods  ============================================================================================

    public static void main(String[] args) {
        runSimulation();
    }


    //  ==  Private methods  ===========================================================================================

    private static void runSimulation() {
        createPhilosophers();
        startThreads();
    }


    private static void createPhilosophers() {
        philosophers = new Philosopher[PHILOSOPHERS_NUMBER];
        createSticks();
        porter = new Semaphore(MAX_PHILOSOPHERS_IN_DINING_ROOM);

        for (int i = 0; i < PHILOSOPHERS_NUMBER; i++) {
            philosophers[i] = new Philosopher(i, sticks[i], sticks[(i + 1) % PHILOSOPHERS_NUMBER], porter);
        }
    }


    private static void createSticks() {
        sticks = new Semaphore[PHILOSOPHERS_NUMBER];

        for (int i = 0; i < sticks.length; i++) {
            sticks[i] = new Semaphore(1);
        }
    }


    private static void startThreads() {
        if (philosophers.length == 0) return;

        for (int i = 0; i < PHILOSOPHERS_NUMBER; i++) {
            philosophers[i].start();
        }
    }
}
