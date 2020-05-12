package com.apicatalog.rdf;

import java.util.stream.Stream;

public interface RdfDataset {

	RdfGraph getDefaultGraph();
	
	void add(String graphName, RdfGraph graph);
	
	Stream<NamedGraph> iterator();
	
	class NamedGraph {

	}
	
}
