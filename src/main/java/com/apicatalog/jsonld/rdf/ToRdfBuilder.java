package com.apicatalog.jsonld.rdf;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.flattening.NodeMap;

public final class ToRdfBuilder {

    // required
    private final NodeMap nodeMap;
    private final Dataset dataset;
    
    // mandatory
    private boolean produceGeneralizedRdf;
    
    private ToRdfBuilder(NodeMap nodeMap, Dataset dataset) {
        this.nodeMap = nodeMap;
        this.dataset = dataset;
    }
    
    
    public static final ToRdfBuilder with(NodeMap nodeMap, Dataset dataset) {
        return new ToRdfBuilder(nodeMap, dataset);
    }
    
    public ToRdfBuilder produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }
    
    public Dataset build() throws JsonLdError {
        return dataset;
    }
    
}
