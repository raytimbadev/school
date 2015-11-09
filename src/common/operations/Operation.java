package common.operations;

import java.util.List;
import org.apache.commons.dbcp2.BasicDataSource;

public interface Operation<T> {
    T invoke(BasicDataSource database); 
    List<Object> getParameters();
}
