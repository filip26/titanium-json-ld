package com.apicatalog.jsonld.rdf;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.IntStream;

import com.apicatalog.jsonld.rdf.RdfDataset.NamedGraph;

public final class RdfComparison {

    final RdfDataset dataset1;
    final RdfDataset dataset2;
    
    private RdfComparison(final RdfDataset dataset1, final RdfDataset dataset2) {
        this.dataset1 = dataset1;
        this.dataset2 = dataset2;
    }
    

    public static final boolean equals(final RdfDataset dataset1, final RdfDataset dataset2) {
        return new RdfComparison(dataset1, dataset2).areIsomorphic();
    }


    // https://www.w3.org/TR/rdf11-concepts/#dfn-dataset-isomorphism
    private boolean areIsomorphic() {

        // compare total number of n-quads
        if (dataset1.size() != dataset2.size()) {
            return false;
        }

        // compare default graphs
        if (!compareGraphs(dataset1.getDefaultGraph(), dataset2.getDefaultGraph())) {
            return false;
        }

        // compare total number of named graphs and triples
        Integer[] triples1 = dataset1.stream().map(NamedGraph::getGraph).map(RdfGraph::size).sorted().toArray(Integer[]::new);
        Integer[] triples2 = dataset2.stream().map(NamedGraph::getGraph).map(RdfGraph::size).sorted().toArray(Integer[]::new);

        if (triples1.length == 0 && triples2.length == 0) {
            return true;
        }
        
        if (!Arrays.equals(triples1, triples2)) {
            return false;
        }

        
        System.out.println("TODO 1");
        
        // TODO Auto-generated method stub
        return false;
    }


    private static final boolean compareGraphs(final RdfGraph graph1, final RdfGraph graph2) {
                
        // compare total number of triples in graphs
        if (graph1.size() != graph2.size()) {
            return false;
        }
        
        // compare triples with no blank node label
        RdfTriple[] triples1 = graph1.stream()
                                .filter(HAS_BLANKS.negate())
                                .toArray(RdfTriple[]::new);
        
        RdfTriple[] triples2 = graph2.stream()
                                .filter(HAS_BLANKS.negate())
                                .toArray(RdfTriple[]::new);

        //TODO sort

        if (triples1.length != triples2.length) {
            return false;
        }
        
        if (!IntStream.range(0, triples1.length).allMatch(i -> compareTriple(triples1[i], triples2[i]))) {
            return false;
        }

        if (triples1.length == graph2.size()) {
            return true;
        }
        System.out.println("TODO 2");


//        if (triples1.length != triples2.length) {
//            return false;
//        }


        
        
        // TODO Auto-generated method stub
        return false;
    }

    private static final boolean compareTriple(final RdfTriple triple1, final RdfTriple triple2) {

        return Objects.equals(triple1.getSubject(), triple2.getSubject())
                && Objects.equals(triple1.getPredicate(), triple2.getPredicate())
                && Objects.equals(triple1.getObject(), triple2.getObject())
                ;
        
    }

    private static final Predicate<RdfTriple> HAS_BLANKS  = t -> t.getObject().isBlankNode() || t.getSubject().isBlankNode(); 
    
}
