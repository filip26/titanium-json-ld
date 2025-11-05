package com.apicatalog.jsonld.tordf;

import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;

class RdfQuadEmitter implements RdfTripleConsumer {

    protected final RdfQuadConsumer consumer;
    protected String graph;

    protected RdfQuadEmitter(RdfQuadConsumer consumer) {
        this.consumer = consumer;
        this.graph = null;
    }

    public static RdfTripleConsumer newEmitter(RdfQuadConsumer consumer) {
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
        consumer.quad(subject, predicate, object, null, null, null, graph);
        return this;
    }

    @Override
    public RdfTripleConsumer triple(String subject, String predicate, String literal, String datatype) throws RdfConsumerException {
        consumer.quad(subject, predicate, literal, datatype, null, null, graph);
        return this;
    }

    @Override
    public RdfTripleConsumer triple(String subject, String predicate, String literal, String language, String direction) throws RdfConsumerException {
        if (direction != null) {
            consumer.quad(subject, predicate, literal, Terms.RDF_I18N_BASE, language, direction, graph);
        } else {
            consumer.quad(subject, predicate, literal, Terms.XSD_STRING, language, direction, graph);
        }
        return this;
    }
}
