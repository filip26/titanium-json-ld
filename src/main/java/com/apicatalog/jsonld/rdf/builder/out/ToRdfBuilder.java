package com.apicatalog.jsonld.rdf.builder.out;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.rdf.Rdf;
import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfGraph;

public final class ToRdfBuilder {

    // required
    private final NodeMap nodeMap;
    private final RdfDataset dataset;
    
    // mandatory
    private boolean produceGeneralizedRdf;
    
    private ToRdfBuilder(NodeMap nodeMap, RdfDataset dataset) {
        this.nodeMap = nodeMap;
        this.dataset = dataset;
    }
    
    public static final ToRdfBuilder with(NodeMap nodeMap, RdfDataset dataset) {
        return new ToRdfBuilder(nodeMap, dataset);
    }
    
    public ToRdfBuilder produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }
    
    public RdfDataset build() throws JsonLdError {
        // 1.
        for (final String graphName : nodeMap.graphs(true)) {

            // 1.1.
            //TODO
            
            // 1.2.
            RdfGraph tripples = null;
            
            if (Keywords.DEFAULT.equals(graphName)) {
                tripples = dataset.getDefaultGraph();
                
            } else {
                
                tripples = Rdf.createGraph();
                dataset.add(graphName, tripples);
            }
            
            // 1.3.
            for (final String subject : nodeMap.subjects(graphName, true)) {
                
                // 1.3.1.
                //TODO
                
                // 1.3.2.
                for (final String property : nodeMap.properties(graphName, subject, true)) {
                    
                    // 1.3.2.1.
                    if (Keywords.TYPE.equals(property)) {
                        
                        //TODO

                    // 1.3.2.2.
                    } else if (Keywords.contains(property)) {
                        continue;
                    }
                    
                }
                
            }

        }
        
        return dataset;
    }
    
}
