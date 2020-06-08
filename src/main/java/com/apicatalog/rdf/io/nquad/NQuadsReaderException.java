package com.apicatalog.rdf.io.nquad;

public class NQuadsReaderException extends Exception {

    private static final long serialVersionUID = -5357042008536435090L;

    public NQuadsReaderException(String message) {
        super(message);
    }
    
    public NQuadsReaderException(Throwable throwable) {
        super(throwable);
    }
    
}
