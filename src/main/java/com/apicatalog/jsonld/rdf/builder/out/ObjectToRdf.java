package com.apicatalog.jsonld.rdf.builder.out;

import java.util.List;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfTriple;

public final class ObjectToRdf {

    // required
    private JsonObject object;
    private List<RdfTriple> triples;
    
    private ObjectToRdf(JsonObject object, List<RdfTriple> triples) {
        this.object = object;
        this.triples = triples;
    }
    
    public static final ObjectToRdf with(JsonObject object, List<RdfTriple> triples) {
        return  new ObjectToRdf(object, triples);
    }
    
    public RdfObject build() throws JsonLdError {
        
        //TODO
        return null;
    }
    
}
