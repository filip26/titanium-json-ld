package com.apicatalog.jsonld.deseralization;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import javax.json.JsonArray;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.lang.RdfVocabulary;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List to RDF Conversion</a>
 *
 */
final class ListToRdf {

    // required
    private JsonArray list;
    private List<RdfTriple> triples;
    private NodeMap nodeMap;
    
    // optional
    private RdfDirection rdfDirection;
    
    private ListToRdf(final JsonArray list, final List<RdfTriple> triples, NodeMap nodeMap) {
        this.list = list;
        this.triples = triples;
        this.nodeMap = nodeMap;
    }
    
    public static final ListToRdf with(final JsonArray list, final List<RdfTriple> triples, NodeMap nodeMap) {
        return new ListToRdf(list, triples, nodeMap);
    }
    
    public ListToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfObject build() throws JsonLdError {
        
        // 1.
        if (JsonUtils.isEmptyArray(list)) {
            return Rdf.createObject(RdfObject.Type.IRI, RdfVocabulary.NIL);
        }

        // 2.
        String[] bnodes = new String[list.size()];

        IntStream.range(0,  bnodes.length).forEach(i -> bnodes[i] = nodeMap.createIdentifier());

        // 3.
        int index = 0;
        for (JsonValue item : list) {
            
            String subject = bnodes[index];
            index++;
            
            // 3.1.
            List<RdfTriple> embeddedTriples = new ArrayList<>();
            
            // 3.2.
            RdfObject object = ObjectToRdf
                                    .with(item.asJsonObject(), embeddedTriples, nodeMap)
                                    .rdfDirection(rdfDirection)
                                    .build();
                                           
            // 3.3.
            if (object != null) {
                triples.add(Rdf.createTriple(   //TODO use statementbuilder
                                    Rdf.createSubject(RdfSubject.Type.BLANK_NODE, subject), 
                                    Rdf.createPredicate(RdfPredicate.Type.IRI, RdfVocabulary.FIRST), 
                                    object
                                    ));
            }

            // 3.4.
            RdfObject rest = (index < bnodes.length) ? Rdf.createObject(RdfObject.Type.BLANK_NODE, bnodes[index]) 
                                        : Rdf.createObject(RdfObject.Type.IRI, RdfVocabulary.NIL)
                                        ;
            
            triples.add(Rdf.createTriple(
                                    Rdf.createSubject(RdfSubject.Type.BLANK_NODE, subject), 
                                    Rdf.createPredicate(RdfPredicate.Type.IRI, RdfVocabulary.REST), 
                                    rest
                                    ));
            
            // 3.5.
            triples.addAll(embeddedTriples);
        }
        
        // 4.
        return Rdf.createObject(RdfObject.Type.BLANK_NODE, bnodes[0]);
    }
}
