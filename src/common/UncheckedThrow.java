package common;

public final class UncheckedThrow {
    private UncheckedThrow(){}

    public static RuntimeException throwUnchecked(final Exception ex){
        UncheckedThrow.<RuntimeException>throwsUnchecked(ex);
        return null; // this code is unreachable
    }

    public static <T extends Exception> RuntimeException throwsUnchecked(
        Exception toThrow
    ) throws T {
        throw (T) toThrow;
    }
}
