package com.apicatalog.rdf;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

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

        // if datasets are empty
        if (dataset1.size() == 0 && dataset2.size() == 0) {
            return true;
        }

        // compare n-quads with no blank node label
        final List<RdfNQuad> nquads1 = dataset1.stream()
                                .filter(HAS_BLANKS.negate())
                                .collect(Collectors.toList());
        
        final List<RdfNQuad> nquads2 = dataset2.stream()
                                .filter(HAS_BLANKS.negate())
                                .collect(Collectors.toList());

        if (nquads1.size() != nquads2.size()) {
            return false;
        }

        // non-blank triples are not the same
        if (!compareNQuads(nquads1, nquads2, null)) {
            return false;
        }

        // datasets have no blank nodes
        if (nquads1.size() == dataset2.size()) {
            return true;
        }

        // n-quads with one or two blank nodes comparison
        final List<RdfNQuad> b1 = dataset1.stream()
                                        .filter(HAS_BLANKS)
                                        .collect(Collectors.toList());

        final List<RdfNQuad> b2 = dataset2.stream()
                                          .filter(HAS_BLANKS)
                                          .collect(Collectors.toList());

        // blank node n-quads count does not match
        if (b1.size() != b2.size()) {
            return false;
        }

        // create mappings from b2 to b1
        final NodeMapper mapper = NodeMapper.create(b2, b1);
  
        int iteration = 0;
        
        while (mapper.hasNext()) {
            if (compareNQuads(b1, b2, mapper.next())) {
                return true;
            }
            
            iteration++;
            
            if (iteration >= 100000) {
                System.out.println("Too many permutations [" + mapper.permutations() + "]");
                return false;
            }
        }

        return false;
    }

    private static final boolean compareNQuads(final List<RdfNQuad> nquads1, final List<RdfNQuad> nquads2, final Map<String, String> mapping) {

        final LinkedList<RdfNQuad> remaining = new LinkedList<>(nquads2);
        
        for (final RdfNQuad nquad1 : nquads1) {
            
            boolean found = false;
            
            for (final RdfNQuad nquad2 : remaining) {
                
                found = compareNQuad(nquad1, nquad2, mapping);
                
                if (found) {
                    remaining.remove(nquad2);
                    break;
                }
            }
        }                
        return remaining.isEmpty();
    }
    
    private static final boolean compareTriple(final RdfTriple triple1, final RdfTriple triple2, final Map<String, String> mapping) {

        return compareSubject(triple1.getSubject(), triple2.getSubject(), mapping)
                && Objects.equals(triple1.getPredicate(), triple2.getPredicate())
                && compareObject(triple1.getObject(), triple2.getObject(), mapping)
                ;
    }
    
    private static final boolean compareNQuad(final RdfNQuad nquad1, final RdfNQuad nquad2, final Map<String, String> mapping) {
        return compareTriple(nquad1, nquad2, mapping)
                && compareGraphName(nquad1.getGraphName(), nquad2.getGraphName(), mapping)
                ;        
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

    private static final boolean compareGraphName(RdfGraphName name1, RdfGraphName name2, Map<String, String> mapping) {

        if (name1 == null || name2 == null) {
            return name1 == name2;            
        }
        
        if (name1.isBlankNode() && name2.isBlankNode()) { 
            
            return Objects.equals(
                            name1.asBlankNode().getLabel(), 
                            mapping != null
                                ? mapping.get(name2.asBlankNode().getLabel())
                                : name2.asBlankNode().getLabel()
                                        );
            
        } else if (name1.isIRI() && name2.isIRI()) {
            
            return Objects.equals(name1.asIRI(), name2.asIRI());
        }
        return false;
    }
    private static final Predicate<RdfNQuad> HAS_BLANKS  = 
                            t -> t.getObject().isBlankNode() 
                                    || t.getSubject().isBlankNode()
                                    || t.getGraphName() != null && t.getGraphName().isBlankNode()
                                    ; 
    
}
