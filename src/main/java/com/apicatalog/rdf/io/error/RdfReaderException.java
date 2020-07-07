package com.apicatalog.rdf.io.error;

public class RdfReaderException extends Exception {

    private static final long serialVersionUID = -5357042008536435090L;

    public RdfReaderException(String message) {
        super(message);
    }
    
    public RdfReaderException(Throwable cause) {
        super(cause);
    }

    public RdfReaderException(String message, Throwable cause) {
        super(message, cause);
    }
}
