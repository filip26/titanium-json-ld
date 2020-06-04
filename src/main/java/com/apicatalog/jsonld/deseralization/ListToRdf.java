package com.apicatalog.jsonld.deseralization;

import java.util.List;

import javax.json.JsonArray;
import javax.json.JsonValue;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.api.Rdf;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List to RDF Conversion</a>
 *
 */
final class ListToRdf {

    // required
    private JsonArray list;
    private List<RdfTriple> triples;
    
    // optional
    private String rdfDirection;
    
    private ListToRdf(final JsonArray list, final List<RdfTriple> triples) {
        this.list = list;
        this.triples = triples;
    }
    
    public static final ListToRdf with(final JsonArray list, final List<RdfTriple> triples) {
        return new ListToRdf(list, triples);
    }
    
    public ListToRdf rdfDirection(String rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfObject build() throws JsonLdError {
        
        // 1.
        if (JsonUtils.isEmptyArray(list)) {
            return Rdf.createObject(IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"));
        }
        
        return null;
    }
    
    
}
