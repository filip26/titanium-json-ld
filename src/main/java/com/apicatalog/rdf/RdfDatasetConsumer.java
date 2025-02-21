package com.apicatalog.rdf;

public class RdfDatasetConsumer implements RdfConsumer {

    protected final RdfDataset dataset;
    
    protected RdfResource graphName;

    public RdfDatasetConsumer() {
        this(Rdf.createDataset());
    }

    public RdfDatasetConsumer(RdfDataset dataset) {
        this.dataset = dataset;
        this.graphName = null;
    }

    public RdfDataset dataset() {
        return dataset;
    }
    
    protected static final RdfResource createResource(String name, boolean blank) {
        return blank ? Rdf.createBlankNode(name) : Rdf.createIRI(name);
    }

    @Override
    public void namedGraph(String graph, boolean blankGraph) {
        this.graphName = createResource(graph, blankGraph);
    }

    @Override
    public void defaultGraph() {
        this.graphName = null;
    }

    @Override
    public void accept(String subject, boolean blankSubject, String predicate, boolean blankPredicate, String literal, String datatype, String language) {
        dataset.add(
                Rdf.createNQuad(
                        createResource(subject, blankSubject),
                        createResource(predicate, blankPredicate),
                        language != null
                                ? Rdf.createLangString(literal, language)
                                : Rdf.createTypedString(literal, datatype),
                        graphName));
    }

    @Override
    public void accept(String subject, boolean blankSubject, String predicate, boolean blankPredicate, String object, boolean blankObject) {
        dataset.add(
                Rdf.createNQuad(
                        createResource(subject, blankSubject),
                        createResource(predicate, blankPredicate),
                        createResource(object, blankObject),
                        graphName));
    }
}
