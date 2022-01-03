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
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.lang.RdfConstants;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class JsonLdToRdf {

    private static final Logger LOGGER = Logger.getLogger(JsonLdToRdf.class.getName());

    // required
    private final NodeMap nodeMap;
    private final RdfDataset dataset;

    // optional
    private boolean produceGeneralizedRdf;
    private RdfDirection rdfDirection;
    private boolean uriValidation;

    private JsonLdToRdf(NodeMap nodeMap, RdfDataset dataset) {
        this.nodeMap = nodeMap;
        this.dataset = dataset;

        this.produceGeneralizedRdf = false;
        this.rdfDirection = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static final JsonLdToRdf with(NodeMap nodeMap, RdfDataset dataset) {
        return new JsonLdToRdf(nodeMap, dataset);
    }

    public JsonLdToRdf produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }

    public JsonLdToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public RdfDataset build() throws JsonLdError {

        // 1.
        for (final String graphName : Utils.index(nodeMap.graphs(), true)) {

            // 1.2.
            final RdfResource rdfGraphName;

            if (Keywords.DEFAULT.equals(graphName)) {
                rdfGraphName = null;

            } else {

                // 1.1.
                if (BlankNode.isWellFormed(graphName)) {

                    rdfGraphName = Rdf.createBlankNode(graphName);

                } else if (UriUtils.isAbsoluteUri(graphName, uriValidation)) {

                    rdfGraphName = Rdf.createIRI(graphName);

                } else {
                    continue;
                }
            }

            // 1.3.
            for (final String subject : Utils.index(nodeMap.subjects(graphName), true)) {

                final RdfResource rdfSubject;

                // 1.3.1.
                if (BlankNode.isWellFormed(subject)) {
                    rdfSubject = Rdf.createBlankNode(subject);

                } else if (UriUtils.isAbsoluteUri(subject, uriValidation)) {
                    rdfSubject = Rdf.createIRI(subject);

                } else {
                    LOGGER.log(Level.WARNING, "Non well-formed subject [{0}] has been skipped.", subject);
                    continue;
                }

                // 1.3.2.
                for (final String property : Utils.index(nodeMap.properties(graphName, subject), true)) {

                    // 1.3.2.1.
                    if (Keywords.TYPE.equals(property)) {

                        for (JsonValue type : nodeMap.get(graphName, subject, property).asJsonArray()) {

                            if (JsonUtils.isNotString(type)) {
                                continue;
                            }

                            final String typeString = ((JsonString)type).getString();

                            final RdfValue rdfObject;

                            if (BlankNode.isWellFormed(typeString)) {
                                rdfObject = Rdf.createBlankNode(typeString);

                            } else if (UriUtils.isAbsoluteUri(typeString, uriValidation)) {
                                rdfObject = Rdf.createIRI(typeString);

                            } else {
                                continue;
                            }

                            dataset.add(Rdf.createNQuad(
                                                rdfSubject,
                                                Rdf.createIRI(RdfConstants.TYPE),
                                                rdfObject,
                                                rdfGraphName
                                            ));
                        }

                    // 1.3.2.2.
                    } else if (!Keywords.contains(property)) {

                        final RdfResource rdfProperty;

                        if (BlankNode.isWellFormed(property)) {
                            rdfProperty = !produceGeneralizedRdf ? Rdf.createBlankNode(property) : null;

                        } else if (UriUtils.isAbsoluteUri(property, uriValidation)) {
                            rdfProperty = Rdf.createIRI(property);

                        } else {
                            rdfProperty = null;
                        }

                        if (rdfProperty != null) {

                            // 1.3.2.5.
                            for (JsonValue item : nodeMap.get(graphName, subject, property).asJsonArray()) {

                                // 1.3.2.5.1.
                                final List<RdfTriple> listTriples = new ArrayList<>();

                                // 1.3.2.5.2.
                                ObjectToRdf
                                        .with(item.asJsonObject(), listTriples, nodeMap)
                                        .rdfDirection(rdfDirection)
                                        .uriValidation(uriValidation)
                                        .build()
                                        .ifPresent(rdfObject ->
                                                            dataset.add(Rdf.createNQuad(
                                                                        rdfSubject,
                                                                        rdfProperty,
                                                                        rdfObject,
                                                                        rdfGraphName
                                                                        )));
                                // 1.3.2.5.3.
                                listTriples.stream()
                                            .map(triple -> Rdf.createNQuad(triple, rdfGraphName))
                                            .forEach(dataset::add);
                            }
                        }
                    }
                }
            }
        }
        return dataset;
    }

    public JsonLdToRdf uriValidation(boolean uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }
}
