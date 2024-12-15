import java.util.ArrayList;

public class Queue<T> implements MyQueue<T> {
    private ArrayList<T> queue;                                                                                         //  Reprezentacja tablicy cyklicznej
    private int capacity;                                                                                               //  Maksymalny rozmiar kolejki
    private int size;                                                                                                   //  Aktualny rozmiar kolejki
    private int front;                                                                                                  //  Indeks pierwszego elementu
    private int rear;                                                                                                   //  Indeks ostatniego elementu

    public Queue(int capacity) {                                                                                        //  Konstruktor parametryczny
        if (capacity > 0) {                                                                                             //  Jeśli zadany maksymalny rozmiar kolejki jest większy niż 0.
            this.capacity = capacity;                                                                                   //  Ustaw pola klasy na odpowiednie wartości
            this.queue = new ArrayList<>(capacity);

            for (int i = 0; i < capacity; i++) {                                                                        //  Wypełnij kolejkę wartością null
                queue.add(null);
            }

            this.front = 0;                                                                                             //  Ustaw indeks początku kolejki na 0
            this.rear = -1;                                                                                             //  Ustaw indeks końca kolejki na -1
            this.size = 0;                                                                                              //  Ustaw aktualny rozmiar kolejki na 0
        }
    }

    @Override
    public void enqueue(T x) throws FullException {
        if (isFull()) throw new FullException("Queue is full.");                                                        //  Jeśli kolejka jest pełna, zgłoś wyjątek

        rear = (rear + 1) % capacity;                                                                                   //  Zaktualizuj ostatni indeks, tak by przesuwał się cyklicznie po tablicy
        queue.set(rear, x);                                                                                             //  Dodaj element na koniec kolejki
        size++;                                                                                                         //  Zwiększ aktualny rozmiar kolejki
    }

    @Override
    public void dequeue() {
        if (!isEmpty()) {                                                                                               //  Jeśli kolejka nie jest pusta,
            queue.set(front, null);                                                                                     //  Ustaw pierwszą wartość w kolejce na null
            front = (front + 1) % capacity;                                                                             //  Zakutalizuj pierwszy indeks, tak by przesuwał się cyklicznie po tablicy
            size--;                                                                                                     //  Zmniejsz aktualny rozmiar kolejki
        }
    }

    @Override
    public T first() throws EmptyException {
        if (isEmpty()) throw new EmptyException("Queue is empty.");                                                     //  Jeśli kolejka jest pusta, zgłoś wyjątek

        return queue.get(front);                                                                                        //  W przeciwnym wypadku, zwróć pierwszy element kolejki
    }

    @Override
    public boolean isEmpty() {
        return size == 0;                                                                                               //  Sprawdź, czy aktualny rozmiar kolejki jest równy 0
    }

    @Override
    public boolean isFull() {
        return size == capacity;                                                                                        //  Sprawdź, czy aktualny rozmiar kolejki jest równy maksymamemu rozmiarowi kolejki
    }
}
