package com.apicatalog.rdf.io.error;

public class UnsupportedContentException extends Exception {

    private static final long serialVersionUID = -5086908507736739151L;
    
    private final  String contenttYpe;
    
    public UnsupportedContentException(final String contentType) {
        super("Content '" + contentType + " is not supported.");
        this.contenttYpe = contentType;
    }
    
    public String getContentType() {
        return contenttYpe;
    }
}
