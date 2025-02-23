package com.apicatalog.rdf;

import java.util.function.Supplier;

public class RdfDatasetSupplier extends RdfQuadAdapter implements Supplier<RdfDataset> {

    protected final RdfDataset dataset;

    public RdfDatasetSupplier() {
        this(Rdf.createDataset());
    }

    public RdfDatasetSupplier(RdfDataset dataset) {
        this.dataset = dataset;
    }

    @Override
    public RdfDataset get() {
        return dataset;
    }

    @Override
    protected void quad(RdfResource subject, RdfResource predicate, RdfValue value, RdfResource graph) {
        dataset.add(Rdf.createNQuad(subject, predicate, value, graph));
    }
}
