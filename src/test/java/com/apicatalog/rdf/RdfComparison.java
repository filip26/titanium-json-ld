package com.apicatalog.rdf;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.apicatalog.rdf.RdfDataset.NamedGraph;

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

        System.out.println("TODO: compare other graphs");
        
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

        if (triples1.size() != triples2.size()) {
            return false;
        }

        // non-blank triples are not the same
        if (!compareTriples(triples1, triples2, null)) {
            return false;
        }

        // graphs have no blank nodes
        if (triples1.size() == graph2.size()) {
            return true;
        }

        // triples with one or two blank nodes comparison
        final List<RdfTriple> b1 = graph1.stream()
                .filter(HAS_BLANKS)
                .collect(Collectors.toList());

        final List<RdfTriple> b2 = graph2.stream()
                .filter(HAS_BLANKS)
                .collect(Collectors.toList());

        // blank node triples count does not match
        if (b1.size() != b2.size()) {
            return false;
        }

        // create mappings from b2 to b1
        final NodeMapper mapper = NodeMapper.create(b2, b1);
        
        //TODO check and stop after permutations limit - set the limit
        
        while (mapper.hasNext()) {
            
            if (compareTriples(b1, b2, mapper.next())) {
                return true;
            }
        }

        return false;
    }
    
    private static final boolean compareTriples(final List<RdfTriple> triples1, final List<RdfTriple> triples2, final Map<String, String> mapping) {

        final LinkedList<RdfTriple> remaining = new LinkedList<>(triples2);
        
        for (final RdfTriple triple1 : triples1) {
            
            boolean found = false;
            
            for (final RdfTriple triple2 : remaining) {
                
                found = compareTriple(triple1, triple2, mapping);
                
                if (found) {
                    remaining.remove(triple2);
                    break;
                }
            }
        }
                
        return remaining.isEmpty();
    }

    private static final boolean compareTriple(final RdfTriple triple1, final RdfTriple triple2, final Map<String, String> mapping) {

        if (!compareSubject(triple1.getSubject(), triple2.getSubject(), mapping)) {
            return false;
        }
        
        if (!Objects.equals(triple1.getPredicate(), triple2.getPredicate())) {
            return false;
        }

        return compareObject(triple1.getObject(), triple2.getObject(), mapping);
    }
    
    private static final boolean compareSubject(RdfSubject subject1, RdfSubject subject2, Map<String, String> mapping) {

        if (subject1.isBlankNode() && subject2.isBlankNode()) { 
            
            return Objects.equals(
                            subject1.asBlankNode().getLabel(), 
                            mapping != null
                                ? mapping.get(subject2.asBlankNode().getLabel())
                                : subject2.asBlankNode().getLabel()
                                        );
            
        } else if (subject1.isIRI() && subject2.isIRI()) {
            
            return Objects.equals(subject1.asIRI(), subject2.asIRI());
        }

        return false;
    }

    private static final boolean compareObject(RdfObject object1, RdfObject object2, Map<String, String> mapping) {
        
        if (object1.isBlankNode() && object2.isBlankNode()) {

            return Objects.equals(
                    object1.asBlankNode().getLabel(), 
                    mapping != null
                        ? mapping.get(object2.asBlankNode().getLabel())
                        : object2.asBlankNode().getLabel()
                                );

        } else if (object1.isIRI() && object2.isIRI()) {

            return Objects.equals(object1.asIRI(), object2.asIRI());
            
        } else if (object1.isLiteral() && object2.isLiteral()) {
            
            return Objects.equals(object1.asLiteral(), object2.asLiteral());
        }
        return false;
    }

    private static final Predicate<RdfTriple> HAS_BLANKS  = t -> t.getObject().isBlankNode() || t.getSubject().isBlankNode(); 
    
}
