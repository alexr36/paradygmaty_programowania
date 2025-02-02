package List11.Zad2.a;

/**
 * The code has been provided in a .zip file attached to the task description
 * (Adjusted only the indentation and some other parts of convention)
 **/


/**
 *  Solution for a)
 **/


class IntCell {
    private int n = 0;
    private boolean is_occupied;


    public synchronized int getN() {
        while (is_occupied) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                e.printStackTrace();
                return Integer.MIN_VALUE;
            }
        }

        is_occupied = true;
        return n;
    }


    public synchronized void setN(int n) {
        this.n = n;
        is_occupied = false;
        notify();
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
