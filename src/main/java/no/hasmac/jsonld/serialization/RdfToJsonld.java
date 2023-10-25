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
package no.hasmac.jsonld.serialization;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.JsonLdOptions.RdfDirection;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.BlankNode;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.lang.LanguageTag;
import no.hasmac.jsonld.lang.Utils;
import no.hasmac.jsonld.uri.UriUtils;
import no.hasmac.rdf.RdfDataset;
import no.hasmac.rdf.RdfGraph;
import no.hasmac.rdf.RdfResource;
import no.hasmac.rdf.RdfTriple;
import no.hasmac.rdf.lang.RdfConstants;

import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class RdfToJsonld {

    // required
    private RdfDataset dataset;

    // optional
    private boolean ordered;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    private boolean useRdfType;
    private boolean uriValidation;

    private JsonLdVersion processingMode;

    // runtime
    private GraphMap graphMap;


    private Map<String, Map<String, Boolean>> compoundLiteralSubjects;
    private Map<String, Reference> referenceOnce;

    private RdfToJsonld(final RdfDataset dataset) {
        this.dataset = dataset;

        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static RdfToJsonld with(final RdfDataset dataset) {
        return new RdfToJsonld(dataset);
    }

    public RdfToJsonld ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public RdfToJsonld rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public RdfToJsonld useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }

    public RdfToJsonld useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }

    public RdfToJsonld processingMode(JsonLdVersion processingMode) {
        this.processingMode = processingMode;
        return this;
    }

    public JsonArray build() throws JsonLdError {

        graphMap = new GraphMap();

        // 3.
        referenceOnce = new LinkedHashMap<>();

        // 4.
        compoundLiteralSubjects = new LinkedHashMap<>();

        // 5.
        step5(Keywords.DEFAULT, dataset.getDefaultGraph());

        for (RdfResource graphName : dataset.getGraphNames()) {
            step5(graphName.getValue(), dataset.getGraph(graphName).orElse(null));
        }

        // 6.
        for (String graphName : graphMap.keys()) {

            // 6.1.
            if (compoundLiteralSubjects.containsKey(graphName)) {

                for (final String cl : compoundLiteralSubjects.get(graphName).keySet()) {

                    // 6.1.1.
                    final Reference clEntry = referenceOnce.get(cl);

                    if (clEntry == null) {
                        continue;
                    }

                    // 6.1.5.
                    final Optional<Map<String, JsonValue>> clNodeValue = graphMap.get(graphName, cl);

                    graphMap.remove(graphName, cl);

                    if (!clNodeValue.isPresent()) {
                        continue;
                    }

                    final Map<String, JsonValue> clNode = clNodeValue.get();

                    final JsonArrayBuilder clArray = JsonProvider.instance().createArrayBuilder();

                    // 6.1.6.
                    for (final JsonValue clReference : graphMap.get(clEntry.graphName, clEntry.subject, clEntry.property).map(JsonValue::asJsonArray).orElse(JsonValue.EMPTY_JSON_ARRAY)) {

                        if (JsonUtils.isNotObject(clReference)) {
                            continue;
                        }

                        final JsonObject clReferenceObject = clReference.asJsonObject();

                        if (!clReferenceObject.containsKey(Keywords.ID) || !cl.equals(clReference.asJsonObject().getString(Keywords.ID))) {
                            continue;
                        }

                        final JsonObjectBuilder clObject = JsonProvider.instance().createObjectBuilder(clReferenceObject);

                        // 6.1.6.1.
                        clObject.remove(Keywords.ID);

                        clObject.add(Keywords.VALUE, JsonUtils.flatten(clNode.get(RdfConstants.VALUE), Keywords.VALUE));

                        // 6.1.6.3.
                        if (clNode.containsKey(RdfConstants.LANGUAGE)) {

                            final JsonValue lang = JsonUtils.flatten(clNode.get(RdfConstants.LANGUAGE), Keywords.VALUE);

                            if (JsonUtils.isNotString(lang) || !LanguageTag.isWellFormed(((JsonString)lang).getString())) {
                                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                            }

                            clObject.add(Keywords.LANGUAGE, lang);
                        }

                        // 6.1.6.4.
                        if (clNode.containsKey(RdfConstants.DIRECTION)) {

                            final JsonValue direction = JsonUtils.flatten(clNode.get(RdfConstants.DIRECTION), Keywords.VALUE);

                            if (JsonUtils.isNotString(direction)
                                    || (!"ltr".equalsIgnoreCase(((JsonString)direction).getString())
                                        && !"rtl".equalsIgnoreCase(((JsonString)direction).getString()))
                                    ) {
                                throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                            }

                            clObject.add(Keywords.DIRECTION, direction);
                        }

                        clArray.add(clObject);
                    }
                    graphMap.set(clEntry.graphName, clEntry.subject, clEntry.property, clArray.build());
                }
            }

            // 6.2.
            if (!graphMap.contains(graphName, RdfConstants.NIL)) {
                continue;
            }

            // 6.4.
            for (Reference usage : graphMap.getUsages(graphName, RdfConstants.NIL)) {

                // 6.4.1.
                Map<String, JsonValue> node = graphMap.get(usage.graphName, usage.subject).orElseGet(() -> Collections.emptyMap());

                // 6.4.2.
                final JsonArrayBuilder list = JsonProvider.instance().createArrayBuilder();
                final List<String> listNodes = new ArrayList<>();

                String nodeId = ((JsonString)node.get(Keywords.ID)).getString();

                // 6.4.3.
                while (RdfConstants.REST.equals(usage.property)
                        && BlankNode.isWellFormed(nodeId)
                        && referenceOnce.get(nodeId) != null
                        && node.containsKey(RdfConstants.FIRST)
                        && node.containsKey(RdfConstants.REST)
                        && node.get(RdfConstants.FIRST).asJsonArray().size() == 1
                        && node.get(RdfConstants.REST).asJsonArray().size() == 1
                        && (node.size() == 3    /* keywords: @id, @first, @last */
                                || (node.size() == 4 && node.containsKey(Keywords.TYPE)
                                    && node.get(Keywords.TYPE).asJsonArray().size() == 1
                                    && node.get(Keywords.TYPE).asJsonArray().contains(JsonProvider.instance().createValue(RdfConstants.LIST))
                                    ))
                        ) {

                    // 6.4.3.1.
                    list.add(0, node.get(RdfConstants.FIRST).asJsonArray().get(0)); // reverse order -> index = 0 see 6.4.5.

                    // 6.4.3.2.
                    listNodes.add(nodeId);

                    // 6.4.3.3.
                    usage = referenceOnce.get(nodeId);

                    // 6.4.3.4.
                    final Optional<Map<String, JsonValue>> nextNode = graphMap.get(usage.graphName, usage.subject);

                    if (!nextNode.isPresent()) {
                        break;
                    }

                    node = nextNode.get();

                    if (!node.containsKey(Keywords.ID)) {
                        break;
                    }

                    nodeId = ((JsonString)node.get(Keywords.ID)).getString();

                    // 6.4.3.5.
                    if (UriUtils.isAbsoluteUri(nodeId, uriValidation)) {
                        break;
                    }
                }

                JsonObject head = usage.value;

                // 6.4.4.
                head.remove(Keywords.ID);

                // 6.4.6.
                head.put(Keywords.LIST, list.build());

                // 6.4.7.
                listNodes.forEach(nid -> graphMap.remove(graphName, nid));
            }
        }

        // 7.
        final JsonArrayBuilder result = JsonProvider.instance().createArrayBuilder();

        // 8.
        for (final String subject : Utils.index(graphMap.keys(Keywords.DEFAULT), ordered)) {

            final Map<String, JsonValue> node = graphMap.get(Keywords.DEFAULT, subject).orElseGet(() -> new LinkedHashMap<>());

            // 8.1.
            if (graphMap.contains(subject)) {

                final JsonArrayBuilder array = JsonProvider.instance().createArrayBuilder();

                for (final String key : Utils.index(graphMap.keys(subject), ordered)) {

                    final Map<String, JsonValue> entry = graphMap.get(subject, key).orElseGet(() -> Collections.emptyMap());

                    if (entry.size() > 1 || !entry.containsKey(Keywords.ID)) {
                        array.add(JsonUtils.toJsonObject(entry));
                    }
                }

                node.put(Keywords.GRAPH, array.build());
            }

            // 8.2.
            if (node.size() > 1 || !node.containsKey(Keywords.ID)) {
                result.add(JsonUtils.toJsonObject(node));
            }
        }
        // 9.
        return result.build();
    }

    private void step5(final String graphName, final RdfGraph graph) throws JsonLdError {

        // 5.3.
        if (!compoundLiteralSubjects.containsKey(graphName)) {
            compoundLiteralSubjects.put(graphName, new LinkedHashMap<>());
        }

        // 5.4.
        if (!Keywords.DEFAULT.equals(graphName) && !graphMap.contains(Keywords.DEFAULT, graphName)) {
            graphMap.set(Keywords.DEFAULT, graphName, Keywords.ID, JsonProvider.instance().createValue(graphName));
        }

        // 5.6.
        final Map<String, Boolean> compoundMap = compoundLiteralSubjects.get(graphName);

        // 5.7.
        for (final RdfTriple triple : graph.toList()) {

            final String subject = triple.getSubject().toString();
            final String predicate = triple.getPredicate().toString();

            // 5.7.1.
            if (!graphMap.contains(graphName, subject)) {
                graphMap.set(graphName, subject, Keywords.ID, JsonProvider.instance().createValue(subject));
            }

            // 5.7.3.
            if (RdfDirection.COMPOUND_LITERAL == rdfDirection
                    && RdfConstants.DIRECTION.equals(predicate)) {

                compoundMap.put(subject, Boolean.TRUE);
            }

            // 5.7.4.
            if ((triple.getObject().isBlankNode() || triple.getObject().isIRI())
                    && !graphMap.contains(graphName, triple.getObject().toString())) {

                graphMap.set(graphName, triple.getObject().toString(), Keywords.ID, JsonProvider.instance().createValue(triple.getObject().toString()));
            }

            // 5.7.5.
            if (!useRdfType && RdfConstants.TYPE.equals(predicate) && !triple.getObject().isLiteral()) {

                final Optional<JsonValue> type = graphMap.get(graphName, subject, Keywords.TYPE);

                if (type.isPresent()) {

                    JsonArray types = type.get().asJsonArray();

                    graphMap.set(graphName, subject, Keywords.TYPE, JsonProvider.instance().createArrayBuilder(types).add(triple.getObject().toString()).build());

                } else {

                    graphMap.set(graphName, subject, Keywords.TYPE, JsonProvider.instance().createArrayBuilder().add(triple.getObject().toString()).build());
                }

                continue;
            }

            // 5.7.6.
            final JsonObject value =
                        RdfToObject
                            .with(triple.getObject(), rdfDirection, useNativeTypes)
                            .processingMode(processingMode)
                            .build();

            final Optional<JsonValue> predicateValue = graphMap.get(graphName, subject, predicate);

            // 5.7.7.
            if (predicateValue.isPresent()) {

                JsonArray array = predicateValue.get().asJsonArray();

                if (!array.contains(value)) {
                    graphMap.set(graphName, subject, predicate, JsonProvider.instance().createArrayBuilder(array).add(value).build());
                }

            // 5.7.8.
            } else {
                graphMap.set(graphName, subject, predicate, JsonProvider.instance().createArrayBuilder().add(value).build());
            }

            // 5.7.9.
            if (triple.getObject().isIRI() && RdfConstants.NIL.equals(triple.getObject().toString())) {

                Reference reference = new Reference();
                reference.graphName = graphName;
                reference.subject = subject;
                reference.property = predicate;
                reference.value = value;

                graphMap.addUsage(graphName, triple.getObject().toString(), reference);

            // 5.7.10.
            } else if (referenceOnce.containsKey(triple.getObject().toString())) {

                referenceOnce.put(triple.getObject().toString(), null);

            // 5.7.11.
            } else if (triple.getObject().isBlankNode()) {

                Reference reference = new Reference();
                reference.graphName = graphName;
                reference.subject = subject;
                reference.property = predicate;
                reference.value = value;

                referenceOnce.put(triple.getObject().toString(), reference);
            }
        }
    }

    protected static class Reference {
        private String graphName;
        private String subject;
        private String property;
        private JsonObject value;
    }

    public RdfToJsonld uriValidation(boolean uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }
}
