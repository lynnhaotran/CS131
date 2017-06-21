import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSet(byte[] v) {
        maxval = 127;
        value = new AtomicIntegerArray(v.length);

        for (int i = 0; i !=  v.length; i++) 
            value.set(i, v[i]);
    }

    GetNSet(byte[] v, byte m) {
        maxval = m;
        value = new AtomicIntegerArray(v.length);

        for (int i = 0; i !=  v.length; i++) 
            value.set(i, v[i]);
    }

    public int size() { return value.length(); }

    public byte[] current() {
        byte[] values = new byte[maxval];
        for (int i = 0; i != value.length(); i++)
            values[i] = (byte) value.get(i);
        return values;
    }


    public boolean swap(int i, int j) {

    if (value.get(i) <= 0 || value.get(j) >= maxval) {
	    return false;
	}

	value.set(i, i - 1);
    value.set(j, j + 1);
    return true;
    }
}