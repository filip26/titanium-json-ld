package com.apicatalog.jsonld.iri;

public final class IRI {

    private final String value;
    
    private IRI(String value) {
        this.value = value;
    }
    
    public static final IRI create(String value) {
        return new IRI(value);
    }

    @Override
    public String toString() {
        return value == null ? "null" : value;
    }

    public static boolean isWellFormed(String subject) {
        // TODO Auto-generated method stub
        return false;
    }
}
