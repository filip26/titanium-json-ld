package com.apicatalog.rdf;

import java.util.HashMap;
import java.util.Map;

public abstract class RdfNQuadConsumer implements RdfConsumer {

    protected final Map<String, RdfResource> resources;

    protected RdfResource graphName;

    public RdfNQuadConsumer() {
        this.resources = new HashMap<>();
        this.graphName = null;
    }

    protected abstract void accept(RdfResource subject, RdfResource predicate, RdfValue value, RdfResource graph);

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
        accept(getResource(subject, blankSubject),
                getResource(predicate, blankPredicate),
                language != null
                        ? Rdf.createLangString(literal, language)
                        : Rdf.createTypedString(literal, datatype),
                graphName);
    }

    @Override
    public void accept(String subject, boolean blankSubject, String predicate, boolean blankPredicate, String object, boolean blankObject) {
        accept(getResource(subject, blankSubject),
                getResource(predicate, blankPredicate),
                getResource(object, blankObject),
                graphName);
    }

    protected final RdfResource getResource(final String name, final boolean blank) {
        return resources.computeIfAbsent(name, arg0 -> blank ? Rdf.createBlankNode(name) : Rdf.createIRI(name));
    }
}
