package List11.Zad1;

/**
 * The code has been provided in a .zip file attached to the task description
 * (Adjusted only the indentation and some other parts of convention)
 */


class IntCell {
    private int n = 0;


    public int getN() {
        return n;
    }


    public void setN(int n) {
        this.n = n;
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


/*
 *  ================================================  ANSWER  ==========================================================
 *  Examples of received results: 240177, 290585, 323658, 210288, 217895.
 *  They are far below 400000. This is caused by the lack of synchronization between threads, which leads to
 *  errors in incrementation of the value n of IntCell object (due to race conditions).
 *  This is caused by overwriting the n value with wrong temp value in run() method.
 *  Each thread loads the current n value to its temp and doesn't wait for the other to finish its operation.
 *  This leads to loss of some of the incremented n values.
 *  ====================================================================================================================
 */