package com.apicatalog.rdf.io.nquad;

public class NQuadsWriterException extends Exception {

    private static final long serialVersionUID = 5939850604399297830L;

    public NQuadsWriterException(String message) {
        super(message);
    }
    
    public NQuadsWriterException(Throwable e) {
        super(e);
    }
    
}
