package com.apicatalog.rdf;

public class RdfDatasetProducer extends RdfNQuadConsumer {

    protected final RdfDataset dataset;

    public RdfDatasetProducer() {
        this(Rdf.createDataset());
    }

    public RdfDatasetProducer(RdfDataset dataset) {
        this.dataset = dataset;
    }

    public RdfDataset dataset() {
        return dataset;
    }

    @Override
    protected void accept(RdfResource subject, RdfResource predicate, RdfValue value, RdfResource graph) {
        dataset.add(Rdf.createNQuad(subject, predicate, value, graph));

    }
}
