package common;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.File;
import java.io.Serializable;
import java.io.IOException;
import java.util.List;
import java.util.Arrays;

/**
 * A durable persistence layer using an n-ary shadowing scheme.
 */
public class SecurePersistenceLayer<T extends Serializable> {
    public final String LOCK_FILE_SUFFIX = ".lock";
    public final String PATH_PREFIX = "persistence/";

    private final List<String> files;

    public SecurePersistenceLayer(List<String> paths) {
        // make sure persistence dir exists.
        new File(PATH_PREFIX).mkdir();

        if(paths.size() == 0)
            throw new RuntimeException("empty list of paths is invalid.");
        files = paths;
    }

    public SecurePersistenceLayer(String path) {
        files = Arrays.asList(new String[] { path });
    }

    private String getFullPath(String path) {
        return PATH_PREFIX + path;
    }

    /**
     * Loads the most recent data dump described by the secure persistence
     * layer's configuration.
     *
     * Will return null if there is no such data dump.
     */
    public T load() throws IOException, ClassNotFoundException {
        final String path = findReadPath();

        if(path == null) {
            Trace.info(
                    String.format(
                        "No data to recover."
                    )
            );
            return null;
        }

        final ObjectInputStream input =
            new ObjectInputStream(new FileInputStream(path));

        final T t = (T)input.readObject();

        input.close();

        Trace.info(
                String.format(
                    "Loaded data from %s",
                    path
                )
        );

        return t;
    }

    /**
     * Persist the given data, overwriting the oldest data dump.
     */
    public void persist(T t) throws IOException {
        final String path = findWritePath();

        if(path == null)
            throw new RuntimeException("no path");

        final ObjectOutputStream output =
            new ObjectOutputStream(new FileOutputStream(path));

        output.writeObject(t);
        output.close();

        final File lockFile = new File(path + LOCK_FILE_SUFFIX);
        if(lockFile.exists())
            lockFile.delete();
        lockFile.createNewFile();

        Trace.info(
                String.format(
                    "Persisted data to %s",
                    path
                )
        );
    }

    private String findWritePath() throws IOException {
        long bestTime = 0;
        String bestPath = null;

        for(final String path : files) {
            final String fullPath = getFullPath(path);
            final File file = new File(fullPath);
            final File lockFile = new File(fullPath + LOCK_FILE_SUFFIX);

            // if there's no lock file, then we can write there
            if(!lockFile.exists())
                return fullPath;

            // if there's a lock file, but no associated data, we have violated
            // an invariant.
            if(!file.exists())
                throw new RuntimeException("wtf");

            // find the oldest lock file
            if(bestPath == null || lockFile.lastModified() < bestTime)
            {
                bestTime = lockFile.lastModified();
                bestPath = fullPath;
            }
        }

        return bestPath;
    }

    private String findReadPath() throws IOException {
        long bestTime = 0;
        String bestPath = null;

        for(final String path : files) {
            final String fullPath = getFullPath(path);
            final File file =
                new File(fullPath);
            final File lockFile =
                new File(fullPath + LOCK_FILE_SUFFIX);

            // if there's no lock file, then we can't read here!
            if(!lockFile.exists())
                continue;

            // if there's a lock file, but no associated data, we have violated
            // an invariant.
            if(!file.exists())
                throw new RuntimeException("wtf");

            // find the most recent lock file
            if(bestPath == null || lockFile.lastModified() > bestTime)
            {
                bestTime = lockFile.lastModified();
                bestPath = fullPath;
            }
        }

        return bestPath;
    }
}
