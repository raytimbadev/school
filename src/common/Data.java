package common;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
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

    private String getLockPath() {
        return path + ".lock";
    }

    public synchronized void persist() throws IOException {
        final File lockFile = new File(getLockPath());

        if(lockFile.exists())
            lockFile.delete();

        final ObjectOutputStream output =
                new ObjectOutputStream(new FileOutputStream(path));
        output.writeObject(this); 

        lockFile.createNewFile();
    }
}
