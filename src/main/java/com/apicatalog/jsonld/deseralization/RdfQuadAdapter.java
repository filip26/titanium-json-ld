package com.apicatalog.jsonld.deseralization;

import java.util.HashMap;
import java.util.Map;

import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;

/**
 * This class is deprecated as of version 1.7.0.
 * <p>
 * Please use
 * <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium RDF
 * Primitives</a> or any other third-party library to materialize RDF
 * primitives.
 * </p>
 *
 * @see <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium
 *      RDF Primitives</a>
 * @deprecated since 1.7.0 - use an alternative RDF primitives library.
 */
@Deprecated
public abstract class RdfQuadAdapter implements RdfQuadConsumer, RdfTripleConsumer {

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
    public RdfQuadConsumer quad(
            String subject,
            String predicate,
            String object,
            String datatype,
            String language,
            String direction,
            String graph) throws RdfConsumerException {

        final RdfValue objectValue;
        if (language != null || direction != null) {
            objectValue = Rdf.createLangString(object, language, direction);

        } else if (datatype != null) {
            objectValue = Rdf.createTypedString(object, datatype);

        } else {
            objectValue = getResource(object);
        }

        quad(getResource(subject),
                getResource(predicate),
                objectValue,
                getResource(graph));

        return this;
    }

    protected final RdfResource getResource(final String name) {
        return name != null
                ? resources.computeIfAbsent(name, arg0 -> name.startsWith("_:") ? Rdf.createBlankNode(name) : Rdf.createIRI(name))
                : null;
    }
}
