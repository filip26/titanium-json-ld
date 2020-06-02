package com.apicatalog.jsonld.rdf;

import java.util.stream.Stream;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(String graphName, RdfGraph graph);

    void add(RdfNQuad nquad);
    
    Stream<NamedGraph> stream();

    class NamedGraph {

    }

}
