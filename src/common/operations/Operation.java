package common.operations;

import common.ItemGroup;

import java.util.List;
import java.util.Hashtable;

public interface Operation<T> {
    T invoke(Hashtable<String, ItemGroup> database); 
    List<Object> getParameters();
}
