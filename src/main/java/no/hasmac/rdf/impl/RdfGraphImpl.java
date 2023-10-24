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
package no.hasmac.rdf.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import no.hasmac.rdf.RdfGraph;
import no.hasmac.rdf.RdfResource;
import no.hasmac.rdf.RdfTriple;
import no.hasmac.rdf.RdfValue;

final class RdfGraphImpl implements RdfGraph {

    private final Map<RdfResource, Map<RdfResource, Set<RdfValue>>> index;

    private final List<RdfTriple> triples;

    protected RdfGraphImpl() {
        this.index = new HashMap<>(1);
        this.triples = new ArrayList<>();
    }

    public void add(final RdfTriple triple) {

        if (triple == null) {
            throw new IllegalArgumentException();
        }

        index
            .computeIfAbsent(triple.getSubject(), x -> new HashMap<>(1))
            .computeIfAbsent(triple.getPredicate(), x -> new HashSet<>(1))
            .add(triple.getObject());

        triples.add(triple);
    }

    @Override
    public boolean contains(final RdfTriple triple) {

        if (triple == null) {
            throw new IllegalArgumentException();
        }

        return index.containsKey(triple.getSubject())
                    && index.get(triple.getSubject()).containsKey(triple.getPredicate())
                    && index.get(triple.getSubject()).get(triple.getPredicate()).contains(triple.getObject())
                    ;
    }

    @Override
    public List<RdfTriple> toList() {
        return triples;
    }
}
