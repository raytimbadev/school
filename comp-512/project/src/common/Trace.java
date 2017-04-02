// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package common;


// A simple wrapper around System.out.println, allows us to disable some of
// the verbose output from RM, TM, and WC if we want.

public class Trace {

    public static void info(String msg) {
        System.out.println(getThreadID() + " INFO: " + msg);
    }

    public static void warn(String msg) {
        System.out.println(getThreadID() + " WARN: " + msg);
    }

    public static void error(String msg) {
        System.err.println(getThreadID() + " ERROR: " + msg);
    }

    private static String getThreadID() {
        return Thread.currentThread().getName();
    }

}
