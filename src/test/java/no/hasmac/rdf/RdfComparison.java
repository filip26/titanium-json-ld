/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package no.hasmac.rdf;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public final class RdfComparison {

    private static final Predicate<RdfNQuad> HAS_BLANKS  =
            t -> t.getObject().isBlankNode()
                    || t.getSubject().isBlankNode()
                    || t.getGraphName().filter(RdfValue::isBlankNode).isPresent()
                    ;

    private final RdfDataset dataset1;
    private final RdfDataset dataset2;

    private RdfComparison(final RdfDataset dataset1, final RdfDataset dataset2) {
        this.dataset1 = dataset1;
        this.dataset2 = dataset2;
    }

    public static boolean equals(final RdfDataset dataset1, final RdfDataset dataset2) {
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
        final List<RdfNQuad> nquads1 = dataset1.toList()
                                            .stream()
                                            .filter(HAS_BLANKS.negate())
                                            .collect(Collectors.toList());

        final List<RdfNQuad> nquads2 = dataset2.toList()
                                            .stream()
                                            .filter(HAS_BLANKS.negate())
                                            .collect(Collectors.toList());

        if (nquads1.size() != nquads2.size()) {
            return false;
        }

        // non-blank n-quads are not the same
        if (!compareNQuads(nquads1, nquads2, null)) {
            return false;
        }

        // datasets have no blank nodes
        if (nquads1.size() == dataset2.size()) {
            return true;
        }

        // n-quads with blank nodes comparison
        final List<RdfNQuad> b1 = dataset1.toList()
                                        .stream()
                                        .filter(HAS_BLANKS)
                                        .collect(Collectors.toList());

        final List<RdfNQuad> b2 = dataset2.toList()
                                        .stream()
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

            if (iteration >=  5000000) {
                System.out.println("Too many permutations [" + mapper.permutations() + "]");
                return false;
            }
        }

        return false;
    }

    private static boolean compareNQuads(final List<RdfNQuad> nquads1, final List<RdfNQuad> nquads2, final Map<String, String> mapping) {

        final LinkedList<RdfNQuad> remaining = new LinkedList<>(nquads2);

        for (final RdfNQuad nquad1 : nquads1) {

            boolean found = false;

            for (final RdfNQuad nquad2 : remaining) {

                found = compareNQuad(nquad2, nquad1, mapping);

                if (found) {
                    remaining.remove(nquad2);
                    break;
                }
            }

            if (!found) {
                return false;
            }
        }
        return remaining.isEmpty();
    }

    private static boolean compareNQuad(final RdfNQuad nquad1, final RdfNQuad nquad2, final Map<String, String> mapping) {
        return compareTriple(nquad1, nquad2, mapping)
                && compareGraphName(nquad1.getGraphName().orElse(null), nquad2.getGraphName().orElse(null), mapping)
                ;
    }

    private static boolean compareTriple(final RdfTriple triple1, final RdfTriple triple2, final Map<String, String> mapping) {

        return compareSubject(triple1.getSubject(), triple2.getSubject(), mapping)
                && Objects.equals(triple1.getPredicate().toString(), triple2.getPredicate().toString())
                && compareObject(triple1.getObject(), triple2.getObject(), mapping)
                ;
    }

    private static boolean compareSubject(RdfResource subject1, RdfResource subject2, Map<String, String> mapping) {

        if (subject1.isBlankNode() && subject2.isBlankNode()) {
            return Objects.equals(
                            subject1.toString(),
                            mapping != null
                                ? mapping.get(subject2.toString())
                                : subject2.toString()
                                        );

        } else if (subject1.isIRI() && subject2.isIRI()) {
            return Objects.equals(subject1.toString(), subject2.toString());
        }
        return false;
    }

    private static boolean compareObject(RdfValue object1, RdfValue object2, Map<String, String> mapping) {

        if (object1.isBlankNode() && object2.isBlankNode()) {

            return Objects.equals(
                    object1.toString(),
                    mapping != null
                        ? mapping.get(object2.toString())
                        : object2.toString()
                                );

        } else if (object1.isIRI() && object2.isIRI()) {
            return Objects.equals(object1.toString(), object2.toString());

        } else if (object1.isLiteral() && object2.isLiteral()) {
            return Objects.equals(object1.asLiteral(), object2.asLiteral());
        }
        return false;
    }

    private static boolean compareGraphName(RdfResource name1, RdfResource name2, Map<String, String> mapping) {

        if (name1 == null || name2 == null) {
            return Objects.equals(name1, name2);
        }

        if (name1.isBlankNode() && name2.isBlankNode()) {

            return Objects.equals(
                            name1.toString(),
                            mapping != null
                                ? mapping.get(name2.toString())
                                : name2.toString()
                                        );

        } else if (name1.isIRI() && name2.isIRI()) {

            return Objects.equals(name1.toString(), name2.toString());
        }
        return false;
    }
}
