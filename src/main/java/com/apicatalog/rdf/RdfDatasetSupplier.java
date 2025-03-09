package com.apicatalog.rdf;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;

public class RdfDatasetSupplier implements RdfQuadConsumer, Supplier<RdfDataset> {

    protected final Map<String, RdfResource> resources;
    protected final RdfDataset dataset;

    public RdfDatasetSupplier() {
        this(Rdf.createDataset(), new HashMap<>());
    }

    public RdfDatasetSupplier(RdfDataset dataset) {
        this(dataset, new HashMap<>());
    }

    public RdfDatasetSupplier(RdfDataset dataset, Map<String, RdfResource> resources) {
        this.dataset = dataset;
        this.resources = resources;
    }

    @Override
    public RdfDataset get() {
        return dataset;
    }

    protected void quad(RdfResource subject, RdfResource predicate, RdfValue value, RdfResource graph) {
        dataset.add(Rdf.createNQuad(subject, predicate, value, graph));
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String object, String datatype, String language, String direction, String graph) throws RdfConsumerException {
        final RdfValue objectValue;
        if (language != null) {
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
