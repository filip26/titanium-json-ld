package com.apicatalog.rdf;

import java.util.HashMap;
import java.util.Map;

public class RdfDatasetConsumer implements RdfConsumer {

    
    protected final Map<String, RdfResource> resources;
    protected final RdfDataset dataset;
    
    protected RdfResource graphName;

    public RdfDatasetConsumer() {
        this(Rdf.createDataset());
    }

    public RdfDatasetConsumer(RdfDataset dataset) {
        this.resources = new HashMap<>();
        this.dataset = dataset;
        this.graphName = null;
    }

    public RdfDataset dataset() {
        return dataset;
    }

    protected final RdfResource getResource(String name, final boolean blank) {
        return resources.computeIfAbsent(name, arg0 -> blank ? Rdf.createBlankNode(name) : Rdf.createIRI(name));
    }
    
    @Override
    public void namedGraph(String graph, boolean blankGraph) {
        this.graphName = getResource(graph, blankGraph);
    }

    @Override
    public void defaultGraph() {
        this.graphName = null;
    }

    @Override
    public void accept(String subject, boolean blankSubject, String predicate, boolean blankPredicate, String literal, String datatype, String language) {
        dataset.add(
                Rdf.createNQuad(
                        getResource(subject, blankSubject),
                        getResource(predicate, blankPredicate),
                        language != null
                                ? Rdf.createLangString(literal, language)
                                : Rdf.createTypedString(literal, datatype),
                        graphName));
    }

    @Override
    public void accept(String subject, boolean blankSubject, String predicate, boolean blankPredicate, String object, boolean blankObject) {
        dataset.add(
                Rdf.createNQuad(
                        getResource(subject, blankSubject),
                        getResource(predicate, blankPredicate),
                        getResource(object, blankObject),
                        graphName));
    }
}
