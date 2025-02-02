package List11.Zad2.b;

import java.util.concurrent.Semaphore;

/**
 * The code has been provided in a .zip file attached to the task description
 * (Adjusted only the indentation and some other parts of convention)
 **/


/**
 *  Solution for b)
 **/


class IntCell {
    private int n = 0;
    private final Semaphore binary_semaphore = new Semaphore(1);


    public int getN() {
        try {
            binary_semaphore.acquire();
            return n;

        }
        catch (InterruptedException e) {
            e.printStackTrace();
            return Integer.MIN_VALUE;
        }
    }


    public void setN(int n) {
        this.n = n;
        binary_semaphore.release();
    }
}



class Count extends Thread {
    private static IntCell n = new IntCell();


    @Override public void run() {
      int temp;

      for (int i = 0; i < 200000; i++) {
        temp = n.getN(); 
        n.setN(temp + 1);
      }
    }


    public static void main(String[] args) {
      Count p = new Count();
      Count q = new Count();
      p.start();
      q.start();

      try {
          p.join();
          q.join();
      }
      catch (InterruptedException e) { }

      System.out.println("The value of n is " + n.getN());
    }
}
