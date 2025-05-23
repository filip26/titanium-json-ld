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
package com.apicatalog.rdf.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;

/**
 * This class is deprecated as of version 1.7.0.
 * <p>
 * Please use
 * <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium RDF
 * Primitives</a> or any other third-party library to materialize RDF
 * primitives.
 * </p>
 *
 * @see <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium
 *      RDF Primitives</a>
 * @deprecated since 1.7.0 - use an alternative RDF primitives library.
 */
@Deprecated
final class MutableRdfGraph implements RdfGraph {

    private final Map<RdfResource, Map<RdfResource, Set<RdfValue>>> index;

    private final List<RdfTriple> triples;

    protected MutableRdfGraph() {
        this.index = new HashMap<>();
        this.triples = new ArrayList<>();
    }

    public void add(final RdfTriple triple) {

        if (triple == null) {
            throw new IllegalArgumentException();
        }

        index
                .computeIfAbsent(triple.getSubject(), x -> new HashMap<>())
                .computeIfAbsent(triple.getPredicate(), x -> new HashSet<>())
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
                && index.get(triple.getSubject()).get(triple.getPredicate()).contains(triple.getObject());
    }

    @Override
    public List<RdfTriple> toList() {
        return triples;
    }
}
