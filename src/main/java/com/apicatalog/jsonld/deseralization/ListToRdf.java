package com.apicatalog.jsonld.deseralization;

import java.util.List;

import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfTriple;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List to RDF Conversion</a>
 *
 */
final class ListToRdf {

    // required
    private JsonValue list;
    private List<RdfTriple> triples;
    
    // optional
    private String rdfDirection;
    
    private ListToRdf(final JsonValue list, final List<RdfTriple> triples) {
        this.list = list;
        this.triples = triples;
    }
    
    public static final ListToRdf with(final JsonValue list, final List<RdfTriple> triples) {
        return new ListToRdf(list, triples);
    }
    
    public ListToRdf rdfDirection(String rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfObject build() throws JsonLdError {
        return null;
    }
    
    
}
