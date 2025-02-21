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
import com.apicatalog.rdf.impl.RdfDatasetConsumer;
import com.apicatalog.rdf.lang.RdfConstants;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class JsonLdToRdf {

    private static final Logger LOGGER = Logger.getLogger(JsonLdToRdf.class.getName());

    // required
    private final NodeMap nodeMap;

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

        for (final String graphName : Utils.index(nodeMap.graphs(), true)) {

            if (Keywords.DEFAULT.equals(graphName)) {
                consumer.defaultGraph();

            } else if (BlankNode.isWellFormed(graphName)) {
                consumer.namedGraph(graphName, true);

            } else if (UriUtils.isAbsoluteUri(graphName, uriValidation)) {
                consumer.namedGraph(graphName, false);

            } else {
                continue;
            }

            for (final String subject : Utils.index(nodeMap.subjects(graphName), true)) {

                boolean blankSubject = false;

                if (BlankNode.isWellFormed(subject)) {
                    blankSubject = true;

                } else if (UriUtils.isNotAbsoluteUri(subject, uriValidation)) {
                    LOGGER.log(Level.WARNING, "Non well-formed subject [{0}] has been skipped.", subject);
                    continue;
                }

                for (final String property : Utils.index(nodeMap.properties(graphName, subject), true)) {

                    if (Keywords.TYPE.equals(property)) {

                        for (final JsonValue type : nodeMap.get(graphName, subject, property).asJsonArray()) {

                            if (JsonUtils.isNotString(type)) {
                                continue;
                            }

                            final String typeString = ((JsonString) type).getString();

                            boolean blankType = false;

                            if (BlankNode.isWellFormed(typeString)) {
                                blankType = true;

                            } else if (UriUtils.isNotAbsoluteUri(typeString, uriValidation)) {
                                continue;
                            }

                            consumer.accept(
                                    subject,
                                    blankSubject,
                                    RdfConstants.TYPE,
                                    false,
                                    typeString,
                                    blankType);
                        }

                    } else if (!Keywords.contains(property)) {

                        boolean blankProperty = false;

                        if (BlankNode.isWellFormed(property) && !produceGeneralizedRdf) {
                            blankProperty = true;

                        } else if (UriUtils.isNotAbsoluteUri(property, uriValidation)) {
                            continue;
                        }

                        for (final JsonValue item : nodeMap.get(graphName, subject, property).asJsonArray()) {

                            ObjectToRdf
                                    .with(item.asJsonObject(), consumer, nodeMap)
                                    .rdfDirection(rdfDirection)
                                    .uriValidation(uriValidation)
                                    .produce(
                                            subject,
                                            blankSubject,
                                            property,
                                            blankProperty);
                        }
                    }
                }
            }
        }
    }

    /**
     * @deprecated since 1.6.0, use {@link #process(RdfConsumer)}.
     * @return
     * @throws JsonLdError
     */
    @Deprecated
    public RdfDataset build() throws JsonLdError {

        if (dataset == null) {
            dataset = Rdf.createDataset();
        }

        process(new RdfDatasetConsumer(dataset));

        return dataset;
    }

    /**
     * @deprecated since 1.5.0, use {@link #uriValidation(UriValidationPolicy)}.
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
