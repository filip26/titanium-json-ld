package com.apicatalog.rdf;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.rdf.api.RdfTripleConsumer;

public abstract class RdfQuadAdapter implements RdfTripleConsumer, RdfQuadConsumer {

    protected final Map<String, RdfResource> resources;

    protected RdfResource graphName;

    public RdfQuadAdapter() {
        this(new HashMap<>());
    }

    public RdfQuadAdapter(Map<String, RdfResource> resources) {
        this.resources = resources;
        this.graphName = null;
    }

    protected abstract void quad(RdfResource subject, RdfResource predicate, RdfValue value, RdfResource graph);

    @Override
    public RdfQuadAdapter namedGraph(String graph) {
        this.graphName = getResource(graph);
        return this;
    }

    @Override
    public RdfQuadAdapter defaultGraph() {
        this.graphName = null;
        return this;
    }

    @Override
    public RdfQuadAdapter triple(String subject, String predicate, String literal, String language, String direction) {
        quad(getResource(subject),
                getResource(predicate),
                Rdf.createLangString(literal, language, direction),
                graphName);
        return this;
    }

    @Override
    public RdfQuadAdapter triple(String subject, String predicate, String literal, String datatype) {
        quad(getResource(subject),
                getResource(predicate),
                Rdf.createTypedString(literal, datatype),
                graphName);
        return this;
    }

    @Override
    public RdfQuadAdapter triple(String subject, String predicate, String object) {
        quad(getResource(subject),
                getResource(predicate),
                getResource(object),
                graphName);
        return this;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String object, String graph) {
        quad(getResource(subject),
                getResource(predicate),
                getResource(object),
                getResource(graph));
        return this;

    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String literal, String datatype, String graph) {
        quad(getResource(subject),
                getResource(predicate),
                Rdf.createTypedString(literal, datatype),
                getResource(graph));
        return this;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String literal, String language, String direction, String graph) {
        quad(getResource(subject),
                getResource(predicate),
                Rdf.createLangString(literal, language, direction),
                getResource(graph));
        return this;
    }

    protected final RdfResource getResource(final String name) {
        return name != null 
                ? resources.computeIfAbsent(name, arg0 -> name.startsWith("_:") ? Rdf.createBlankNode(name) : Rdf.createIRI(name))
                : null;
    }
}
