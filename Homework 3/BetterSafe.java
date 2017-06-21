import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class BetterSafe implements State {
    private byte[] value;
    private byte maxval;
    private Lock lock = new ReentrantLock();

    BetterSafe(byte[] v) { value = v; maxval = 127; }

    BetterSafe(byte[] v, byte m) { value = v; maxval = m; }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {

    lock.lock();
    try {
        if (value[i] <= 0 || value[j] >= maxval)
            return false;
        value[i]--;
        value[j]++;
        return true;
    }
    finally {
        lock.unlock();
    }

    }
}


    public boolean swap(int i, int j) {

        int previ = value.get(i);
        int prevj = value.get(j);

         if (prevj <= 0 || previ >= maxval) {
                return false;
            }

        while(!value.compareAndSet(i, previ, previ + 1))
            previ = value.get(i);

        while(!value.compareAndSet(j, prevj, prevj - 1))
            prevj = value.get(j);
        return true;
    }
