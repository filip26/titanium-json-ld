package com.apicatalog.jsonld.rdf;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

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
        
        if (graph1.size() == 0 && graph2.size() == 0) {
            return true;
        }
        
        // compare triples with no blank node label
        final List<RdfTriple> triples1 = graph1.stream()
                                .filter(HAS_BLANKS.negate())
                                .collect(Collectors.toList());
        
        final List<RdfTriple> triples2 = graph2.stream()
                                .filter(HAS_BLANKS.negate())
                                .collect(Collectors.toList());

        //TODO sort

        if (triples1.size() != triples2.size()) {
            return false;
        }

        if (!compareTriples(triples1, triples2)) {
            return false;
        }

        if (triples1.size() == graph2.size()) {
            return true;
        }

        // try plain comparison
        final List<RdfTriple> b1 = graph1.stream()
                .filter(HAS_BLANKS)
                .collect(Collectors.toList());

        final List<RdfTriple> b2 = graph2.stream()
                .filter(HAS_BLANKS)
                .collect(Collectors.toList());

        if (b1.size() != b2.size()) {
            return false;
        }

        if (compareTriples(b1, b2)) {
            return true;
        }
        
        System.out.println("TODO 2");
        
        // TODO Auto-generated method stub
        return false;
    }
    
    private static final boolean compareTriples(final List<RdfTriple> triples1, final List<RdfTriple> triples2) {

        final LinkedList<RdfTriple> remaining = new LinkedList<>(triples2);
        
        for (final RdfTriple triple1 : triples1) {
            
            boolean found = false;
            
            for (final RdfTriple triple2 : remaining) {
                
                found = compareTriple(triple1, triple2);
                
                if (found) {
                    remaining.remove(triple2);
                    break;
                }
            }
        }
        
        
        return remaining.isEmpty();
    }

    private static final boolean compareTriple(final RdfTriple triple1, final RdfTriple triple2) {

        return Objects.equals(triple1.getSubject(), triple2.getSubject())
                && Objects.equals(triple1.getPredicate(), triple2.getPredicate())
                && Objects.equals(triple1.getObject(), triple2.getObject())
                ;
        
    }

    private static final Predicate<RdfTriple> HAS_BLANKS  = t -> t.getObject().isBlankNode() || t.getSubject().isBlankNode(); 
    
}
