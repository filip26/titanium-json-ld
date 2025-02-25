package com.apicatalog.rdf.api;

public class RdfQuadEmitter implements RdfTripleConsumer {

    protected final RdfQuadConsumer consumer;
    protected String graph;

    protected RdfQuadEmitter(RdfQuadConsumer consumer) {
        this.consumer = consumer;
        this.graph = null;
    }

    public static RdfTripleConsumer newInstance(RdfQuadConsumer consumer) {
        return new RdfQuadEmitter(consumer);
    }

    @Override
    public RdfTripleConsumer defaultGraph() {
        this.graph = null;
        return this;
    }

    @Override
    public RdfTripleConsumer namedGraph(String graph) {
        this.graph = graph;
        return this;
    }

    @Override
    public RdfTripleConsumer triple(String subject, String predicate, String object) throws RdfConsumerException {
        consumer.quad(subject, predicate, object, graph);
        return this;
    }

    @Override
    public RdfTripleConsumer triple(String subject, String predicate, String literal, String datatype) throws RdfConsumerException {
        consumer.quad(subject, predicate, literal, datatype, graph);
        return this;
    }

    @Override
    public RdfTripleConsumer triple(String subject, String predicate, String literal, String language, String direction) throws RdfConsumerException {
        consumer.quad(subject, predicate, subject, language, direction, graph);
        return this;
    }
}
