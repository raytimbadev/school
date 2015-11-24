package common;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

public class Data 
    extends HashMap<String, ItemGroup>
    implements
        Serializable,
        Map<String, ItemGroup> {
}
