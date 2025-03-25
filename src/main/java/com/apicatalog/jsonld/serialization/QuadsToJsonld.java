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
package com.apicatalog.jsonld.serialization;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.serialization.GraphMap.Reference;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonParser;

public final class QuadsToJsonld implements RdfQuadConsumer {

    // optional
    private boolean ordered;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    private boolean useRdfType;
    private UriValidationPolicy uriValidation;
    private JsonLdVersion processingMode;

    // runtime
    private GraphMap graphMap;
    private Map<String, Map<String, Boolean>> compoundLiteralSubjects;
    private Map<String, Reference> referenceOnce;

    public QuadsToJsonld() {
        this.graphMap = new GraphMap();
        this.referenceOnce = new LinkedHashMap<>();
        this.compoundLiteralSubjects = new LinkedHashMap<>();

        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public QuadsToJsonld ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public QuadsToJsonld rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public QuadsToJsonld useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }

    public QuadsToJsonld useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }

    public QuadsToJsonld processingMode(JsonLdVersion processingMode) {
        this.processingMode = processingMode;
        return this;
    }

    public QuadsToJsonld uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    /**
     * Resets the consumer to an empty state, allowing the same instance to process
     * different datasets.
     * 
     * @return the current {@link QuadsToJsonld} instance after resetting
     */
    public QuadsToJsonld reset() {
        this.graphMap = new GraphMap();
        this.referenceOnce = new LinkedHashMap<>();
        this.compoundLiteralSubjects = new LinkedHashMap<>();
        return this;
    }

    /**
     * Generates a new JSON-LD representation based on the received quads.
     * 
     * @return a {@link JsonArray} containing the generated JSON-LD data
     * @throws JsonLdError if an error occurs during JSON-LD generation
     */
    public JsonArray toJsonLd() throws JsonLdError {

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

                            if (JsonUtils.isNotString(lang) || !LanguageTag.isWellFormed(((JsonString) lang).getString())) {
                                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                            }

                            clObject.add(Keywords.LANGUAGE, lang);
                        }

                        // 6.1.6.4.
                        if (clNode.containsKey(RdfConstants.DIRECTION)) {

                            final JsonValue direction = JsonUtils.flatten(clNode.get(RdfConstants.DIRECTION), Keywords.VALUE);

                            if (JsonUtils.isNotString(direction)
                                    || (!"ltr".equalsIgnoreCase(((JsonString) direction).getString())
                                            && !"rtl".equalsIgnoreCase(((JsonString) direction).getString()))) {
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

                String nodeId = ((JsonString) node.get(Keywords.ID)).getString();

                // 6.4.3.
                while (RdfConstants.REST.equals(usage.property)
                        && BlankNode.isWellFormed(nodeId)
                        && referenceOnce.get(nodeId) != null
                        && node.containsKey(RdfConstants.FIRST)
                        && node.containsKey(RdfConstants.REST)
                        && node.get(RdfConstants.FIRST).asJsonArray().size() == 1
                        && node.get(RdfConstants.REST).asJsonArray().size() == 1
                        && (node.size() == 3 /* keywords: @id, @first, @last */
                                || (node.size() == 4 && node.containsKey(Keywords.TYPE)
                                        && node.get(Keywords.TYPE).asJsonArray().size() == 1
                                        && node.get(Keywords.TYPE).asJsonArray().contains(JsonProvider.instance().createValue(RdfConstants.LIST))))) {

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

                    nodeId = ((JsonString) node.get(Keywords.ID)).getString();

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

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String object, String datatype, String language, String direction, String graph) throws RdfConsumerException {

        final String graphName = graph == null ? Keywords.DEFAULT : graph;

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
        if (!RdfQuadConsumer.isLiteral(datatype, language, direction)
                && !graphMap.contains(graphName, object)) {

            graphMap.set(graphName, object, Keywords.ID, JsonProvider.instance().createValue(object));
        }

        // 5.7.5.
        if (!useRdfType && RdfConstants.TYPE.equals(predicate) && !RdfQuadConsumer.isLiteral(datatype, language, direction)) {

            final Optional<JsonValue> type = graphMap.get(graphName, subject, Keywords.TYPE);

            if (type.isPresent()) {

                JsonArray types = type.get().asJsonArray();

                graphMap.set(graphName, subject, Keywords.TYPE, JsonProvider.instance().createArrayBuilder(types).add(object).build());

            } else {

                graphMap.set(graphName, subject, Keywords.TYPE, JsonProvider.instance().createArrayBuilder().add(object).build());
            }

            return this;
        }

        // 5.7.6.
        final JsonObject value = toObject(object, datatype, language, direction);

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
        if (RdfConstants.NIL.equals(object)) {

            Reference reference = new Reference();
            reference.graphName = graphName;
            reference.subject = subject;
            reference.property = predicate;
            reference.value = value;

            graphMap.addUsage(graphName, object, reference);

            // 5.7.10.
        } else if (referenceOnce.containsKey(object)) {

            referenceOnce.put(object, null);

            // 5.7.11.
        } else if (RdfQuadConsumer.isBlank(object)) {

            Reference reference = new Reference();
            reference.graphName = graphName;
            reference.subject = subject;
            reference.property = predicate;
            reference.value = value;

            referenceOnce.put(object, reference);
        }

        return this;
    }

    JsonObject toObject(String object, String datatype, String langTag, String direction) throws RdfConsumerException {

        // 1.
        if (!RdfQuadConsumer.isLiteral(datatype, langTag, direction)) {
            return new RefJsonObject(JsonProvider.instance().createObjectBuilder().add(Keywords.ID, object).build());
        }

        final JsonObjectBuilder result = JsonProvider.instance().createObjectBuilder();

        // 2.2.
        JsonValue convertedValue = null;

        // 2.3.
        String type = null;

        // 2.4.
        if (useNativeTypes) {

            if (datatype != null) {

                // 2.4.1.
                if (XsdConstants.STRING.equals(datatype)) {
                    convertedValue = JsonProvider.instance().createValue(object);

                    // 2.4.2.
                } else if (XsdConstants.BOOLEAN.equals(datatype)) {

                    if ("true".equalsIgnoreCase(object)) {

                        convertedValue = JsonValue.TRUE;

                    } else if ("false".equalsIgnoreCase(object)) {

                        convertedValue = JsonValue.FALSE;

                    } else {

                        type = XsdConstants.BOOLEAN;
                    }

                    // 2.4.3.
                } else if (XsdConstants.INTEGER.equals(datatype) || XsdConstants.INT.equals(datatype) || XsdConstants.LONG.equals(datatype)) {

                    convertedValue = JsonProvider.instance().createValue(Long.parseLong(object));

                } else if (XsdConstants.DOUBLE.equals(datatype) || XsdConstants.FLOAT.equals(datatype)) {

                    convertedValue = JsonProvider.instance().createValue(Double.parseDouble(object));

                } else if (datatype != null) {

                    type = datatype;
                }
            }

            // 2.5.
        } else if (processingMode != JsonLdVersion.V1_0
                && RdfConstants.JSON.equals(datatype)) {

            try (JsonParser parser = JsonProvider.instance().createParser(new StringReader(object))) {

                parser.next();

                convertedValue = parser.getValue();
                type = Keywords.JSON;

            } catch (Exception e) {
                throw new RdfConsumerException(new JsonLdError(JsonLdErrorCode.INVALID_JSON_LITERAL, e));
            }

            // 2.6.
        } else if (RdfDirection.I18N_DATATYPE == rdfDirection
                && datatype != null
                && datatype.startsWith(RdfConstants.I18N_BASE)) {

            convertedValue = JsonProvider.instance().createValue(object);

            String langId = datatype.substring(RdfConstants.I18N_BASE.length());

            int directionIndex = langId.indexOf('_');

            if (directionIndex > 1) {

                result.add(Keywords.LANGUAGE, JsonProvider.instance().createValue(langId.substring(0, directionIndex)));
                result.add(Keywords.DIRECTION, JsonProvider.instance().createValue(langId.substring(directionIndex + 1)));

            } else if (directionIndex == 0) {

                result.add(Keywords.DIRECTION, JsonProvider.instance().createValue(langId.substring(1)));

            } else if (directionIndex == -1) {

                result.add(Keywords.LANGUAGE, JsonProvider.instance().createValue(langId));
            }

            // 2.7.
        } else if (langTag != null) {

            result.add(Keywords.LANGUAGE, Json.createValue(langTag));

            // 2.8.
        } else if (datatype != null
                && !XsdConstants.STRING.equals(datatype)) {

            type = datatype;
        }

        // 2.9.
        result.add(Keywords.VALUE, (convertedValue != null)
                ? convertedValue
                : JsonProvider.instance().createValue(object));

        // 2.10.
        if (type != null) {
            result.add(Keywords.TYPE, JsonProvider.instance().createValue(type));
        }

        // 2.11.
        return result.build();
    }
}