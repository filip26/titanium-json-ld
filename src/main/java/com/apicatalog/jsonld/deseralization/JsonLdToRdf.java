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
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfConsumer;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.lang.RdfConstants;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class JsonLdToRdf {

    private static final Logger LOGGER = Logger.getLogger(JsonLdToRdf.class.getName());

    // required
    private final NodeMap nodeMap;

//    private RdfConsumer consumer;

    // optional
    private boolean produceGeneralizedRdf;
    private RdfDirection rdfDirection;
    private UriValidationPolicy uriValidation;

    // deprecated
    private RdfDataset dataset;
    
    private JsonLdToRdf(NodeMap nodeMap, RdfDataset dataset) {
        this.nodeMap = nodeMap;
        this.dataset = dataset;

        this.produceGeneralizedRdf = false;
        this.rdfDirection = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    @Deprecated
    public static final JsonLdToRdf with(NodeMap nodeMap, RdfDataset dataset) {
        return new JsonLdToRdf(nodeMap, dataset);
    }

    public static final JsonLdToRdf with(NodeMap nodeMap) {
        return new JsonLdToRdf(nodeMap, null);
    }

    public JsonLdToRdf produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }

    public JsonLdToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public void process(RdfConsumer consumer) throws JsonLdError {

        // 1.
        for (final String graphName : Utils.index(nodeMap.graphs(), true)) {

            // 1.2.
//            final RdfResource rdfGraphName;
//            final String rdfGraphName;
//            final boolean rdfBlankGraphName;

            if (Keywords.DEFAULT.equals(graphName)) {
//                rdfGraphName = null;
                consumer.defaultGraph();

            } else {

                // 1.1.
                if (BlankNode.isWellFormed(graphName)) {

                    // rdfGraphName = Rdf.createBlankNode(graphName);
//                    rdfGraphName = graphName;
//                    rdfBlankGraphName = true;
                    consumer.namedGraph(graphName, true);

                } else if (UriUtils.isAbsoluteUri(graphName, uriValidation)) {

//                    rdfGraphName = Rdf.createIRI(graphName);
//
//                    rdfGraphName = graphName;
//                    rdfBlankGraphName = false;
                    consumer.namedGraph(graphName, false);
                } else {
                    continue;
                }
            }

            // 1.3.
            for (final String subject : Utils.index(nodeMap.subjects(graphName), true)) {

//                final RdfResource rdfSubject;
                final String rdfSubject;
                boolean rdfBlankSubject = false;

                // 1.3.1.
                if (BlankNode.isWellFormed(subject)) {
//                    rdfSubject = Rdf.createBlankNode(subject);
                    rdfSubject = subject;
                    rdfBlankSubject = true;

                } else if (UriUtils.isAbsoluteUri(subject, uriValidation)) {
//                    rdfSubject = Rdf.createIRI(subject);
                    rdfSubject = subject;

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

                            final String typeString = ((JsonString) type).getString();

//                            final RdfValue rdfObject;
                            final String rdfObject;
                            boolean rdfBlankObject = false;

                            if (BlankNode.isWellFormed(typeString)) {
//                                rdfObject = Rdf.createBlankNode(typeString);
                                rdfObject = typeString;
                                rdfBlankObject = true;

                            } else if (UriUtils.isAbsoluteUri(typeString, uriValidation)) {
//                                rdfObject = Rdf.createIRI(typeString);
                                rdfObject = typeString;

                            } else {
                                continue;
                            }

                            consumer.accept(
                                    rdfSubject,
                                    rdfBlankSubject,
                                    RdfConstants.TYPE,
                                    false,
                                    rdfObject,
                                    rdfBlankObject);

//                            dataset.add(Rdf.createNQuad(
//                                                rdfSubject,
//                                                Rdf.createIRI(RdfConstants.TYPE),
//                                                rdfObject,
//                                                rdfGraphName
//                                            ));
                        }

                        // 1.3.2.2.
                    } else if (!Keywords.contains(property)) {

//                        final RdfResource rdfProperty;
                        final String rdfProperty;
                        boolean rdfBlankProperty = false;

                        if (BlankNode.isWellFormed(property)) {
//                            rdfProperty = !produceGeneralizedRdf ? Rdf.createBlankNode(property) : null;
                            rdfProperty = !produceGeneralizedRdf ? property : null;
                            rdfBlankProperty = true;

                        } else if (UriUtils.isAbsoluteUri(property, uriValidation)) {
                            // rdfProperty = Rdf.createIRI(property);
                            rdfProperty = property;

                        } else {
                            rdfProperty = null;
                        }

                        if (rdfProperty != null) {

                            // 1.3.2.5.
                            for (final JsonValue item : nodeMap.get(graphName, subject, property).asJsonArray()) {

                                // 1.3.2.5.1.
                                final List<RdfTriple> listTriples = new ArrayList<>();

                                // 1.3.2.5.2.
                                ObjectToRdfProducer
//                                        .with(item.asJsonObject(), listTriples, nodeMap)
                                        .with(item.asJsonObject(), consumer, nodeMap)
                                        .rdfDirection(rdfDirection)
                                        .uriValidation(uriValidation)
                                        .produce(
                                                rdfSubject,
                                                rdfBlankSubject,
                                                rdfProperty,
                                                rdfBlankProperty);
//                                      .ifPresent(rdfObject ->
//                                      consumer.accept(
//                                              rdfSubject,
//                                              rdfBlankSubject,
//                                              rdfProperty,
//                                              rdfObject,
//                                              rdfBlankObject,
//                                              rdfGraphName,
//                                              rdfBlankGraphName
//                                              ));

//                                        .build()
//                                        .ifPresent(rdfObject ->
//                                        consumer.accept(
//                                                rdfSubject,
//                                                rdfBlankSubject,
//                                                rdfProperty,
//                                                rdfObject,
//                                                rdfBlankObject,
//                                                rdfGraphName,
//                                                rdfBlankGraphName
//                                                ));

//                                                            dataset.add(Rdf.createNQuad(
//                                                                        rdfSubject,
//                                                                        rdfProperty,
//                                                                        rdfObject,
//                                                                        rdfGraphName
//                                                                        )));
                                // 1.3.2.5.3.
//                                listTriples.stream()
//                                            .map(triple -> Rdf.createNQuad(triple, rdfGraphName))
//                                            .forEach(dataset::add);
                            }
                        }
                    }
                }
            }
        }
    }

    @Deprecated
    public RdfDataset build() throws JsonLdError {

        if (dataset == null) {
            dataset = Rdf.createDataset();    
        }
        
        process(new RdfConsumer() {

            RdfResource graph;

            // TODO make static final
            RdfResource createResource(String name, boolean blank) {
                return blank ? Rdf.createBlankNode(name) : Rdf.createIRI(name);
            }

            @Override
            public void namedGraph(String graph, boolean blankGraph) {
                this.graph = createResource(graph, blankGraph);
            }

            @Override
            public void defaultGraph() {
                this.graph = null;
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
                                graph));
            }

            @Override
            public void accept(String subject, boolean blankSubject, String predicate, boolean blankPredicate, String object, boolean blankObject) {
                dataset.add(
                        Rdf.createNQuad(
                                createResource(subject, blankSubject),
                                createResource(predicate, blankPredicate),
                                createResource(object, blankObject),
                                graph));
            }
        });

        return dataset;
    }

    /**
     * @deprecated since 1.5.0, use
     *             <code>JsonLdToRdf#uriValidation(com.apicatalog.jsonld.uri.UriValidationPolicy)</code>
     */
    @Deprecated
    public JsonLdToRdf uriValidation(boolean enabled) {
        return uriValidation(enabled ? UriValidationPolicy.Full : UriValidationPolicy.SchemeOnly);
    }

    public JsonLdToRdf uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }
}
