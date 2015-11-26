package common;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class Data
    extends HashMap<String, ItemGroup>
    implements
        Serializable,
        Map<String, ItemGroup> {
    private final String path;

    public Data(String path) {
        this.path = path;
    }

    public String getPath() {
        return path;
    }

    private Data(String path, Map<String, ItemGroup> existingData) {
        super(existingData);
        this.path = path;
    }
}
