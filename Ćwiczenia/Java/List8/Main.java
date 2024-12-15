public class Main {
    public static void main(String[] args) throws FullException, EmptyException {
        Queue<Integer> new_queue = new Queue<>(3);

        System.out.println("isEmpty: " + new_queue.isEmpty());
        new_queue.enqueue(2);
        System.out.println("Enqueued: 2");
        new_queue.enqueue(4);
        System.out.println("Enqueued: 4");
        System.out.println("isEmpty: " + new_queue.isEmpty());
        System.out.println("isFull: " + new_queue.isFull());
        System.out.println("first: " + new_queue.first());
        new_queue.dequeue();
        System.out.println("Dequeued: 2");
        System.out.println("first: " + new_queue.first());
        System.out.println("isEmpty: " + new_queue.isEmpty());
        new_queue.enqueue(7);
        System.out.println("Enqueued: 7");
        new_queue.enqueue(8);
        System.out.println("Enqueued: 8");
        System.out.println("first: " + new_queue.first());
        System.out.println("isFull: " + new_queue.isFull());
        new_queue.enqueue(9);
        System.out.println("Enqueued: 9");
    }
}
