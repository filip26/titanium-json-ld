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
package com.apicatalog.jsonld.deseralization;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.lang.RdfConstants;

import jakarta.json.JsonArray;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List to RDF Conversion</a>
 *
 */
final class ListToRdf {

    // required
    private JsonArray list;
    private List<RdfTriple> triples;
    private NodeMap nodeMap;

    // optional
    private RdfDirection rdfDirection;
    private boolean uriValidation;

    private ListToRdf(final JsonArray list, final List<RdfTriple> triples, NodeMap nodeMap) {
        this.list = list;
        this.triples = triples;
        this.nodeMap = nodeMap;

        // default values
        this.rdfDirection = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static final ListToRdf with(final JsonArray list, final List<RdfTriple> triples, NodeMap nodeMap) {
        return new ListToRdf(list, triples, nodeMap);
    }

    public ListToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public RdfValue build() throws JsonLdError {

        // 1.
        if (JsonUtils.isEmptyArray(list)) {
            return Rdf.createIRI(RdfConstants.NIL);
        }

        // 2.
        final String[] bnodes = new String[list.size()];

        IntStream.range(0,  bnodes.length).forEach(i -> bnodes[i] = nodeMap.createIdentifier());

        // 3.
        int index = 0;
        for (final JsonValue item : list) {

            final String subject = bnodes[index];
            index++;

            // 3.1.
            final List<RdfTriple> embeddedTriples = new ArrayList<>();

            // 3.2.
            ObjectToRdf
                .with(item.asJsonObject(), embeddedTriples, nodeMap)
                .rdfDirection(rdfDirection)
                .uriValidation(uriValidation)
                .build()
                .ifPresent(object ->
                                triples.add(Rdf.createTriple(
                                                Rdf.createBlankNode(subject),
                                                Rdf.createIRI(RdfConstants.FIRST),
                                                object)));

            // 3.4.
            final RdfValue rest = (index < bnodes.length) ? Rdf.createBlankNode(bnodes[index])
                                        : Rdf.createIRI(RdfConstants.NIL)
                                        ;

            triples.add(Rdf.createTriple(
                                    Rdf.createBlankNode(subject),
                                    Rdf.createIRI(RdfConstants.REST),
                                    rest
                                    ));

            // 3.5.
            triples.addAll(embeddedTriples);
        }

        // 4.
        return Rdf.createBlankNode(bnodes[0]);
    }

    public ListToRdf uriValidation(boolean uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }
}
