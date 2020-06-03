package com.apicatalog.jsonld.rdf.builder.out;

import java.util.List;

import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.rdf.Rdf;
import com.apicatalog.jsonld.rdf.RdfLiteral;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfTriple;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#deserialize-json-ld-to-rdf-algorithm">Object to RDF Conversion</a>
 *
 */
public final class ObjectToRdf {

    // required
    private JsonObject item;
    private List<RdfTriple> triples;
    
    private ObjectToRdf(JsonObject item, List<RdfTriple> triples) {
        this.item = item;
        this.triples = triples;
    }
    
    public static final ObjectToRdf with(JsonObject item, List<RdfTriple> triples) {
        return  new ObjectToRdf(item, triples);
    }
    
    public RdfObject build() throws JsonLdError {
        
        // 1. - 2.
        if (NodeObject.isNodeObject(item)) {
            
            JsonValue id = item.get(Keywords.ID);
            
            if (JsonUtils.isNotString(id)) {
                return null;
            }
            
            String idString = ((JsonString)id).getString();
            
            if (BlankNode.isWellFormed(idString)) {
                return Rdf.createObject(BlankNode.create(idString));
            }
            if (IRI.isWellFormed(idString)) {
                return Rdf.createObject(IRI.create(idString));
            }
            return null;
        }
        
        // 3.
        if (ListObject.isListObject(item)) {
            //TODO
            return null;
        }

        // 4.
        if (!ValueObject.isValueObject(item)) {
            //TODO
            return null;
        }
        
        JsonValue value = item.get(Keywords.VALUE);
        
        // 5.
        JsonValue datatype = item.get(Keywords.TYPE);
        
        String valueString = null;
        String datatypeString = null;
        
        // 6.
        if (JsonUtils.isNotNull(datatype)) {
            //TODO
        }
        
        // 7.
        //TODO
        
        // 8.
        
        // 9.
        if (JsonUtils.isTrue(value) || JsonUtils.isFalse(value)) {
            
            //TODO
            
        // 10. - 11.
        } else if (JsonUtils.isNumber(value)) {
            
            //TODO
            
        // 12.
        } else if (JsonUtils.isNull(datatype)) {
            
            datatypeString = item.containsKey(Keywords.LANGUAGE)
                                ? "rdf:langString"
                                : "xsd:string"
                                ;
        }
        
        if (valueString == null) {
            valueString = ((JsonString)value).getString();
        }
        
        if (datatypeString == null) {
            datatypeString = ((JsonString)datatype).getString();
        }
        
        RdfLiteral rdfLiteral = null;
        
        // 13.
        if (item.containsKey(Keywords.DIRECTION) /*TODO rdfDirection */) {

            //TODO

            
        // 14.
        } else {
            if ("xsd:string".equals(datatypeString)) {
                rdfLiteral = Rdf.createLitteral(valueString);
                
            } else {
                rdfLiteral = Rdf.createLitteral(valueString, IRI.create(datatypeString));
            }
        }
        
        // 15.
        return rdfLiteral != null ? Rdf.createObject(rdfLiteral) : null;
    }
    
}
