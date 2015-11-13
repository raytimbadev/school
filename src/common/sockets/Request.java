package common.sockets;

import common.ResourceManager;
import common.UncheckedThrow;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.nio.ByteBuffer;
import java.util.ArrayList;

public class Request implements Serializable {
    final String method;
    final Object[] parameters;
    final Boolean[] primitives;
    final Class[] types;

    public Request(
            final String method,
            final Object[] parameters,
            final Boolean[] primitives)
    throws NoSuchFieldException, IllegalAccessException {
        if(parameters.length != primitives.length)
            throw new IllegalArgumentException("Array lengths don't match.");

        types = new Class[parameters.length];
        for(int i = 0; i < parameters.length; i++) {
            final Class type = parameters[i].getClass();

            if(primitives[i])
                types[i] = (Class)type.getField("TYPE").get(null);
            else
                types[i] = type;
        }

        this.method = method;
        this.parameters = parameters;
        this.primitives = primitives;
    }

    /**
     * Invokes the method represented by this Request on a {@link ResourceManager}
     *
     * @param manager The ResourceManager to invoke the request on.
     */
    public Object invoke(ResourceManager manager)
    throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        return manager.getClass()
            .getMethod(method, types)
            .invoke(manager, parameters);
    }

    public static class RequestParser {
        public static Request parseStream(final InputStream stream)
        throws IOException, ClassNotFoundException {
            final ObjectInputStream ois = new ObjectInputStream(stream);
            return (Request)ois.readObject();
        }
    }

    public static class RequestBuilder {
        final ArrayList<Boolean> primitives;
        final ArrayList<Object> parameters;
        String method;

        public RequestBuilder() {
            primitives = new ArrayList<Boolean>();
            parameters = new ArrayList<Object>();
        }

        /**
         * Adds a reference value as a parameter.
         *
         * @param object The object to add.
         */
        public <T extends Object> RequestBuilder parameter(T object) {
            primitives.add(false);
            parameters.add(object);
            return this;
        }

        /**
         * Adds a primitive value as a parameter.
         *
         * @param object The primitive to add, wrapped in a reference value.
         */
        public <T extends Object> RequestBuilder primitive(T object) {
            primitives.add(true);
            parameters.add(object);
            return this;
        }

        /**
         * Adds a reference value, optionally wrapping a primitive, as a
         * parameter.
         *
         * @param object The object to add.
         * @param primitive Whether the object is wrapping a primitive value.
         */
        public <T extends Object> RequestBuilder parameter(
                T object,
                boolean primitive) {
            primitives.add(primitive);
            parameters.add(object);
            return this;
        }

        public RequestBuilder withMethod(String name) {
            method = name;
            return this;
        }

        public Request build() {
            try {
                return new Request(
                        method,
                        parameters.toArray(),
                        primitives.toArray(new Boolean[primitives.size()])
                );
            }
            catch(Exception e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }
    }
}
