package com.apicatalog.rdf;

public class RdfQuadEmitter implements RdfTripleConsumer {

    protected final RdfQuadConsumer consumer;
    protected String graph;

    public RdfQuadEmitter(RdfQuadConsumer consumer) {
        this.consumer = consumer;
        this.graph = null;
    }

    @Override
    public RdfQuadEmitter defaultGraph() {
        this.graph = null;
        return this;
    }

    @Override
    public RdfQuadEmitter namedGraph(String graph) {
        this.graph = graph;
        return this;
    }

    @Override
    public RdfQuadEmitter triple(String subject, String predicate, String object) {
        consumer.quad(subject, predicate, object, graph);
        return this;
    }

    @Override
    public RdfQuadEmitter triple(String subject, String predicate, String literal, String datatype) {
        consumer.quad(subject, predicate, literal, datatype, graph);
        return this;
    }

    @Override
    public RdfQuadEmitter triple(String subject, String predicate, String literal, String language, String direction) {
        consumer.quad(subject, predicate, subject, language, direction, graph);
        return this;
    }
}
