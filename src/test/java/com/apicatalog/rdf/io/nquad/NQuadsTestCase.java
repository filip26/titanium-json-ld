package com.apicatalog.rdf.io.nquad;

import javax.json.JsonObject;

import com.apicatalog.jsonld.lang.Keywords;

public final class NQuadsTestCase {

    public enum Type { POSITIVE, NEGATIVE }
    
    private final String name;
    private final String comment;
    private final Type type;

    public NQuadsTestCase(final String name, final String comment, final Type type) {
        this.name = name;
        this.comment = comment;
        this.type = type;
    }
    
    public String getName() {
        return name;
    }
    
    public String getComment() {
        return comment;
    }
    
    public Type getType() {
        return type;
    }

    public static final NQuadsTestCase of(JsonObject json) {
        
        Type type = null;
        
        if ("http://www.w3.org/ns/rdftest#TestNQuadsPositiveSyntax".equals(json.getJsonArray(Keywords.TYPE).getString(0))) {
            
            type = Type.POSITIVE;
            
        } else if ("http://www.w3.org/ns/rdftest#TestNQuadsNegativeSyntax".equals(json.getJsonArray(Keywords.TYPE).getString(0))) {
            
            type = Type.NEGATIVE;
        }
        
        final String name = json.getJsonArray("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name").getJsonObject(0).getString(Keywords.VALUE);

        final String comment = json.getJsonArray("http://www.w3.org/2000/01/rdf-schema#comment").getJsonObject(0).getString(Keywords.VALUE);

        return new NQuadsTestCase(name, comment, type);
    }
    
    
}
