package com.apicatalog.rdf.io.error;

public class RdfWriterException extends Exception {

    private static final long serialVersionUID = 5939850604399297830L;

    public RdfWriterException(String message) {
        super(message);
    }
    
    public RdfWriterException(Throwable cause) {
        super(cause);
    }
    
    public RdfWriterException(String message, Throwable cause) {
        super(message, cause);
    }
}
