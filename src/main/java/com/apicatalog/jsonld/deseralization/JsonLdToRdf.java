package com.apicatalog.jsonld.deseralization;

import java.util.LinkedList;
import java.util.List;

import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.lang.RdfContants;

public final class JsonLdToRdf {

    // required
    private final NodeMap nodeMap;
    private final RdfDataset dataset;
    
    // optional
    private boolean produceGeneralizedRdf;
    private RdfDirection rdfDirection;
    
    private JsonLdToRdf(NodeMap nodeMap, RdfDataset dataset) {
        this.nodeMap = nodeMap;
        this.dataset = dataset;
        
        this.produceGeneralizedRdf = false;
        this.rdfDirection = null;
    }
    
    public static final JsonLdToRdf with(NodeMap nodeMap, RdfDataset dataset) {
        return new JsonLdToRdf(nodeMap, dataset);
    }
    
    public JsonLdToRdf produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }

    public JsonLdToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public RdfDataset build() throws JsonLdError {
        // 1.
        for (final String graphName : nodeMap.graphs(true)) {

            // 1.2.
            final RdfGraphName rdfGraphName;
            
            if (Keywords.DEFAULT.equals(graphName)) {

                rdfGraphName = null;
                
            } else {

                // 1.1.
                if (BlankNode.isWellFormed(graphName)) {
                    
                    rdfGraphName = Rdf.createGraphName(RdfGraphName.Type.BLANK_NODE, graphName);
                    
                } else if (UriUtils.isAbsoluteUri(graphName)) {
                 
                    rdfGraphName = Rdf.createGraphName(RdfGraphName.Type.IRI, graphName);
                    
                } else {
                    continue;
                }
            }
            
            // 1.3.
            for (final String subject : nodeMap.subjects(graphName, true)) {
                
                RdfSubject rdfSubject = null;

                // 1.3.1.
                if (BlankNode.isWellFormed(subject)) {
     
                    rdfSubject = Rdf.createSubject(RdfSubject.Type.BLANK_NODE, subject);
                    
                } else if (UriUtils.isAbsoluteUri(subject)) {
                    rdfSubject = Rdf.createSubject(RdfSubject.Type.IRI, subject);
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
                                rdfObject = Rdf.createObject(RdfObject.Type.BLANK_NODE, typeString);
                                
                            } else if (UriUtils.isAbsoluteUri(typeString)) {
                                rdfObject = Rdf.createObject(RdfObject.Type.IRI, typeString);
                                
                            } else {
                                continue;
                            }

                            dataset.add(Rdf.createNQuad(
                                                rdfSubject,
                                                Rdf.createPredicate(RdfPredicate.Type.IRI, RdfContants.TYPE),
                                                rdfObject,
                                                rdfGraphName
                                            ));
                        }

                    // 1.3.2.2.
                    } else if (!Keywords.contains(property) 
                                    && !(BlankNode.isWellFormed(property) && !produceGeneralizedRdf) 
                                    && UriUtils.isURI(property)) {

                        // 1.3.2.5.
                        for (JsonValue item : nodeMap.get(graphName, subject, property).asJsonArray()) {
                        
                            // 1.3.2.5.1.
                            List<RdfTriple> listTriples = new LinkedList<>();

                            // 1.3.2.5.2.                            
                            RdfObject rdfObject = ObjectToRdf
                                                    .with(item.asJsonObject(), listTriples, nodeMap)
                                                    .rdfDirection(rdfDirection)
                                                    .build();
                            
                            if (rdfObject != null) {
                                dataset.add(Rdf.createNQuad(
                                                        rdfSubject,
                                                        Rdf.createPredicate(RdfPredicate.Type.IRI, property),
                                                        rdfObject,
                                                        rdfGraphName
                                                    ));
                            }
                            
                            // 1.3.2.5.3.
                            listTriples.stream()
                                        .map(t -> Rdf.createNQuad(t.getSubject(), t.getPredicate(), t.getObject(), rdfGraphName))
                                        .forEach(dataset::add);
                        }
                    }   
                }   
            }
        }
        
        return dataset;
    }
    
}
