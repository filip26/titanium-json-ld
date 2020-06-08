package com.apicatalog.rdf.io.error;

import com.apicatalog.rdf.io.RdfFormat;

public class UnsupportedFormatException extends Exception {

    private static final long serialVersionUID = -5086908507736739151L;
    
    private final RdfFormat format;
    
    public UnsupportedFormatException(RdfFormat format) {
        super("Format '" + format.name() + " is not supported.");
        this.format = format;
    }
    
    public RdfFormat getFormat() {
        return format;
    }
}
