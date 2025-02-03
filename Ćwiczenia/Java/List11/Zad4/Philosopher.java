package List11.Zad4;

import java.util.Random;
import java.util.concurrent.Semaphore;

import static List11.Zad4.Main.program_counter;


public class Philosopher extends Thread {
    //  ==  Class fields  ==============================================================================================

    //  Constants
    private static final Random rand = new Random();
    private static final int MINIMAL_MEDITATING_TIME = 3000;    //  3s
    private static final int MINIMAL_EATING_TIME = 2000;        //  2s


    //  Variables
    private Semaphore porter;
    private Semaphore left_stick;
    private Semaphore right_stick;
    private int id;



    //  ==  Constructors  ==============================================================================================

    public Philosopher(int id, Semaphore left_stick, Semaphore right_stick, Semaphore porter) {
        this.id = id;
        this.left_stick = left_stick;
        this.right_stick = right_stick;
        this.porter = porter;
    }


    //  ==  Public methods  ============================================================================================

    @Override
    public void run() {
        try {
            while (program_counter > 0) {
                meditate();

                //  The porter lets a philosopher in if possible
                porter.acquire();

                //  A philosopher gets to the table and proceeds to eat
                handlePhilosopherAtTheTable();

                //  The porter acknowledges that there is one more free slot
                porter.release();

                //  Decrement the remaining time quants number
                program_counter--;
            }

            System.out.println("The simulation time is over.");
            System.exit(0);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            //  In case of failure free the resources
            returnSticks();
            porter.release();
        }
    }


    //  ==  Private methods  ===========================================================================================

    private void handlePhilosopherAtTheTable() throws InterruptedException {
        pickSticks();
        eat();
        returnSticks();
    }


    private void meditate() throws InterruptedException {
        System.out.println("Philosopher " + id + " is meditating...");

        //  Simulate meditating time
        Thread.sleep(rand.nextInt(MINIMAL_MEDITATING_TIME) + MINIMAL_MEDITATING_TIME);
        System.out.println("Philosopher " + id + " is taking a break in meditating.");
    }


    private void eat() throws InterruptedException {
        System.out.println("Philosopher " + id + " is eating...");

        //  Simulate eating time
        Thread.sleep(rand.nextInt(MINIMAL_EATING_TIME) + MINIMAL_EATING_TIME);
        System.out.println("Philosopher " + id + " finished eating.");
    }


    //  Methods for handling the sticks

    private void pickLeftStick() throws InterruptedException {
        left_stick.acquire();
        System.out.println("Philosopher " + id + " picked the left stick.");
    }


    private void pickRightStick() throws InterruptedException {
        right_stick.acquire();
        System.out.println("Philosopher " + id + " picked the right stick.");
    }


    private void pickSticks() throws InterruptedException {
        pickLeftStick();
        pickRightStick();
    }


    private void returnLeftStick() {
        System.out.println("Philosopher " + id + " returned the left stick.");
        left_stick.release();
    }


    private void returnRightStick() {
        System.out.println("Philosopher " + id + " returned the right stick.");
        right_stick.release();
    }


    private void returnSticks() {
        returnLeftStick();
        returnRightStick();
    }
}
