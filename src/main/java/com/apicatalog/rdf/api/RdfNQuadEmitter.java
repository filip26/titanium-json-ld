package com.apicatalog.rdf.api;

import com.apicatalog.rdf.lang.RdfConstants;

public class RdfNQuadEmitter implements RdfQuadConsumer {

    protected final RdfNQuadConsumer consumer;

    protected RdfNQuadEmitter(RdfNQuadConsumer consumer) {
        this.consumer = consumer;
    }

    public static RdfQuadConsumer newInstance(RdfNQuadConsumer consumer) {
        return new RdfNQuadEmitter(consumer);
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String object, String graph) {
        consumer.nquad(subject, predicate, object, graph);
        return this;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String literal, String datatype, String graph) {
        consumer.nquad(subject, predicate, literal, datatype, null, graph);
        return this;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String literal, String language, String direction, String graph) {
        if (direction != null) {
            consumer.nquad(
                    subject,
                    predicate,
                    literal,
                    RdfConstants.I18N_BASE
                            .concat(language)
                            .concat("_")
                            .concat(direction),
                    null,
                    graph);
        } else {
            consumer.nquad(subject, predicate, literal, RdfConstants.LANG_STRING, language, graph);
        }

        return this;
    }
}
