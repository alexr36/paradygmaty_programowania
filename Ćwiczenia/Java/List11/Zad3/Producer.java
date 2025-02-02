package List11.Zad3;

/**
 * The code has been provided in a .zip file attached to the task description
 * (Adjusted only the indentation and some other parts of convention)
 */

public class Producer extends Thread {
    final private Produce buf;


    public Producer(String name, Produce buf) {
    	super(name);
        this.buf = buf;
    }


    @Override public void run() {
      for (int i = 1; i <= 100; i++) {
         System.out.println(getName() + " producing " + i);
         buf.put(i);
      }
    }
}

