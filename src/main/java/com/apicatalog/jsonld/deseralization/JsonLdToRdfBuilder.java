package com.apicatalog.jsonld.deseralization;

import java.util.LinkedList;
import java.util.List;

import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.api.Rdf;
import com.apicatalog.uri.UriUtils;

public final class JsonLdToRdfBuilder {

    // required
    private final NodeMap nodeMap;
    private final RdfDataset dataset;
    
    // optional
    private boolean produceGeneralizedRdf;
    private String rdfDirection;
    
    private JsonLdToRdfBuilder(NodeMap nodeMap, RdfDataset dataset) {
        this.nodeMap = nodeMap;
        this.dataset = dataset;
        
        this.produceGeneralizedRdf = false;
        this.rdfDirection = null;
    }
    
    public static final JsonLdToRdfBuilder with(NodeMap nodeMap, RdfDataset dataset) {
        return new JsonLdToRdfBuilder(nodeMap, dataset);
    }
    
    public JsonLdToRdfBuilder produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }

    public JsonLdToRdfBuilder rdfDirection(String rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public RdfDataset build() throws JsonLdError {
        // 1.
        for (final String graphName : nodeMap.graphs(true)) {

            // 1.1.
            //TODO
            
            // 1.2.
            RdfGraph triples = null;
            
            if (Keywords.DEFAULT.equals(graphName)) {
                triples = dataset.getDefaultGraph();
                
            } else {
                
                triples = Rdf.createGraph();
                dataset.add(graphName, triples);
            }
            
            // 1.3.
            for (final String subject : nodeMap.subjects(graphName, true)) {
                
                RdfSubject rdfSubject = null;
                
                // 1.3.1.
                if (BlankNode.isWellFormed(subject)) {
                    rdfSubject = Rdf.createSubject(BlankNode.create(subject));
                    
                } else if (IRI.isWellFormed(subject)) {
                    rdfSubject = Rdf.createSubject(IRI.create(subject));
                }
                
                if (rdfSubject == null) {
                    continue;
                }
                
                // 1.3.2.
                for (final String property : nodeMap.properties(graphName, subject, true)) {
                    
                    // 1.3.2.1.
                    if (Keywords.TYPE.equals(property)) {
                        
                        for (JsonValue type : nodeMap.get(graphName, subject, property).asJsonArray()) { 
                        
                            if (JsonUtils.isNotString(type)) {
                                continue;
                            }
                            
                            final String typeString = ((JsonString)type).getString();

                            RdfObject rdfObject = null;
                            
                            if (BlankNode.isWellFormed(typeString)) {
                                rdfObject = Rdf.createObject(BlankNode.create(typeString));
                                
                            } else if (IRI.isWellFormed(typeString)) {
                                rdfObject = Rdf.createObject(IRI.create(typeString));
                                
                            } else {
                                //TODO literal
                            }
                            
                            if (rdfObject == null) {
                                continue;
                            }
                                                        
                            triples.add(Rdf.createTriple(
                                                rdfSubject,
                                                IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                                                rdfObject
                                            ));
                        }

                    // 1.3.2.2.
                    } else if (Keywords.contains(property)
                            // 1.3.2.3.
                            || (BlankNode.isWellFormed(property) && !produceGeneralizedRdf)
                            // 1.3.2.4.
                            //TODO
                            ) {
                        continue;
                        
                    // 1.3.2.5.
                    } else if (BlankNode.isWellFormed(property) || UriUtils.isURI(property)) {

                        for (JsonValue item : nodeMap.get(graphName, subject, property).asJsonArray()) {
                        
                            // 1.3.2.5.1.
                            List<RdfTriple> listTriples = new LinkedList<>();
                            
                            // 1.3.2.5.2.                            
                            RdfObject rdfObject = ObjectToRdf.with(item.asJsonObject(), listTriples).rdfDirection(rdfDirection).build();

                            if (rdfObject != null) {
                                triples.add(Rdf.createTriple(
                                                        rdfSubject,
                                                        IRI.create(property),
                                                        rdfObject
                                                    ));
                            }
                            
                            // 1.3.2.5.3.
                            listTriples.forEach(triples::add);
                        }
                    }
                    
                }
                
            }

        }
        
        return dataset;
    }
    
}
