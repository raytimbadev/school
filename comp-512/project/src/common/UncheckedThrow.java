package common;

public final class UncheckedThrow {
    private UncheckedThrow(){}

    public static RuntimeException throwUnchecked(final Throwable ex){
        UncheckedThrow.<RuntimeException>throwsUnchecked(ex);
        return null; // this code is unreachable
    }

    public static <T extends Throwable> RuntimeException throwsUnchecked(
        Throwable toThrow
    ) throws T {
        throw (T) toThrow;
    }
}
