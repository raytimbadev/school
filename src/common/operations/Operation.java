package common.operations;

import java.util.List;

public interface Operation<T> {
    T invoke(); 
    List<Object> getParameters();
}
