/*
 * Copyright 2025 the original author or authors.
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
package com.apicatalog.jsonld.fromrdf;

import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.fromrdf.GraphMap.Reference;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.lang.LanguageTag;
import com.apicatalog.web.uri.UriUtils;
import com.apicatalog.web.uri.UriValidationPolicy;

/**
 * QuadsToJsonld implements {@link RdfQuadConsumer} and provides functionality
 * to transform an RDF quad set into a JSON-LD document in expanded form.
 * <p>
 * The transformation process involves processing compound literal subjects,
 * lists, blank nodes, and constructing JSON-LD representations for both default
 * and named graphs.
 * </p>
 * <p>
 * This class can be configured using methods such as {@link #ordered(boolean)},
 * {@link #rdfDirection(RdfDirection)}, {@link #useNativeTypes(boolean)},
 * {@link #useRdfType(boolean)}, {@link #mode(JsonLdVersion)}, and
 * {@link #uriValidation(UriValidationPolicy)}.
 * </p>
 * <p>
 * Key methods include:
 * <ul>
 * <li>{@link #reset()} to clear the internal state for reuse.</li>
 * <li>{@link #toJsonLd()} to generate the JSON-LD output from the processed
 * quads.</li>
 * <li>{@link #quad(String, String, String, String, String, String, String)} to
 * ingest a single RDF quad.</li>
 * </ul>
 * </p>
 * 
 * @see RdfQuadConsumer
 * @since 1.7.0
 */
public class QuadsToJsonLd implements RdfQuadConsumer {

    public static final Map<String, Function<String, Object>> DEFAULT_NATIVE_ADAPTERS = Map.of(
            Terms.XSD_STRING, s -> s,
            Terms.XSD_BOOLEAN, QuadsToJsonLd::toBoolean,
            Terms.XSD_INT, QuadsToJsonLd::toLong,
            Terms.XSD_INTEGER, QuadsToJsonLd::toLong,
            Terms.XSD_LONG, QuadsToJsonLd::toLong,
            Terms.XSD_FLOAT, QuadsToJsonLd::toDouble,
            Terms.XSD_DOUBLE, QuadsToJsonLd::toDouble);

    // optional
    protected boolean ordered;
    protected RdfDirection rdfDirection;
    protected boolean useRdfType;
    protected boolean useXsdDecimal;
    protected UriValidationPolicy uriValidation;
    protected JsonLdVersion processingMode;

    protected NodeParser jsonParser;
    protected Map<String, Function<String, Object>> nativeTypes;

    // runtime
    protected GraphMap graphMap;
    protected Map<String, Map<String, Boolean>> compoundLiteralSubjects;
    protected Map<String, Reference> referenceOnce;

    public QuadsToJsonLd() {
        this.graphMap = new GraphMap();
        this.referenceOnce = new LinkedHashMap<>();
        this.compoundLiteralSubjects = new LinkedHashMap<>();

        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.jsonParser = null;
        this.nativeTypes = Map.of();
        this.useXsdDecimal = false;
        this.useRdfType = false;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
        this.processingMode = JsonLdVersion.V1_1;
    }

    /**
     * Configures whether the output should be ordered.
     * 
     * @param ordered true if ordering is required; false otherwise
     * @return this instance for chaining
     */
    public QuadsToJsonLd ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    /**
     * Sets the RDF direction configuration.
     * 
     * @param rdfDirection the RDF direction (e.g., COMPOUND_LITERAL)
     * @return this instance for chaining
     */
    public QuadsToJsonLd rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    /**
     * Configures whether to use native types for literal conversion.
     * 
     * @param useNativeTypes true to use native types; false otherwise
     * @return this instance for chaining
     */
    public QuadsToJsonLd useNativeTypes(boolean useNativeTypes) {
        this.nativeTypes = useNativeTypes
                ? DEFAULT_NATIVE_ADAPTERS
                : Map.of();
        return this;
    }

    public QuadsToJsonLd useNativeTypes(Map<String, Function<String, Object>> converters) {
        this.nativeTypes = converters != null
                ? converters
                : Map.of();
        return this;
    }

    public QuadsToJsonLd useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }

    /**
     * Sets the JSON-LD processing mode.
     * 
     * @param processingMode the processing mode (e.g., V1_0, V1_1)
     * @return this instance for chaining
     */
    public QuadsToJsonLd mode(JsonLdVersion processingMode) {
        this.processingMode = processingMode;
        return this;
    }

    /**
     * Sets the URI validation policy.
     * 
     * @param uriValidation the URI validation policy
     * @return this instance for chaining
     */
    public QuadsToJsonLd uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    public QuadsToJsonLd jsonParser(NodeParser jsonParser) {
        this.jsonParser = jsonParser;
        return this;
    }

    public QuadsToJsonLd options(JsonLdOptions options) {
        this.ordered = options.isOrdered();
        this.rdfDirection = options.rdfDirection();
        this.useNativeTypes(options.isUseNativeTypes());
        this.useRdfType = options.isUseRdfType();
        this.uriValidation = options.uriValidation();
        this.processingMode = options.mode();
        return this;
    }

    /**
     * Resets the consumer to an empty state, allowing the same instance to process
     * different datasets.
     * 
     * @return the current {@link QuadsToJsonLd} instance after resetting
     */
    public QuadsToJsonLd reset() {
        this.graphMap.clear();
        this.referenceOnce.clear();
        this.compoundLiteralSubjects.clear();
        return this;
    }

    /**
     * Generates a new JSON-LD representation based on the received quads.
     * <p>
     * This method processes all the quads stored in the internal graph map and
     * transforms them into a JSON-LD document in expanded form.
     * </p>
     * 
     * @return a collection containing the generated JSON-LD data
     * @throws JsonLdException if an error occurs during JSON-LD generation
     */
    public Collection<?> toJsonLd() throws JsonLdException {

        // 6.
        for (final var graphName : graphMap.keys()) {

            // 6.1.
            if (compoundLiteralSubjects.containsKey(graphName)) {

                for (final String cl : compoundLiteralSubjects.get(graphName).keySet()) {

                    // 6.1.1.
                    final Reference clEntry = referenceOnce.get(cl);

                    if (clEntry == null) {
                        continue;
                    }

                    // 6.1.5.
                    final var clNodeValue = graphMap.get(graphName, cl);

                    graphMap.remove(graphName, cl);

                    if (!clNodeValue.isPresent()) {
                        continue;
                    }

                    final var clNode = clNodeValue.get();

                    final var clArray = new ArrayList<Object>();

                    // 6.1.6.
                    for (final var clReference : graphMap
                            .get(
                                    clEntry.graphName(),
                                    clEntry.subject(),
                                    clEntry.property())
                            .map(NativeAdapter::asCollection)
                            .orElse(List.of())) {

                        if (clReference == null || !(clReference instanceof Map)) {
                            continue;
                        }

                        @SuppressWarnings({ "unchecked" })
                        final var clReferenceObject = (Map<String, Object>) clReference;

                        if (!clReferenceObject.containsKey(Keywords.ID) || !cl.equals(clReferenceObject.get(Keywords.ID))) {
                            continue;
                        }

                        final var clObject = new LinkedHashMap<String, Object>(clReferenceObject);

                        // 6.1.6.1.
                        clObject.remove(Keywords.ID);

                        clObject.put(Keywords.VALUE, flatten(clNode.get(Terms.RDF_VALUE), Keywords.VALUE));

                        // 6.1.6.3.
                        if (clNode.containsKey(Terms.RDF_LANGUAGE)) {

                            final var lang = flatten(clNode.get(Terms.RDF_LANGUAGE), Keywords.VALUE);

                            if (!(lang instanceof String langString)
                                    || !LanguageTag.isWellFormed(langString)) {
                                throw new JsonLdException(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                            }

                            clObject.put(Keywords.LANGUAGE, lang);
                        }

                        // 6.1.6.4.
                        if (clNode.containsKey(Terms.RDF_DIRECTION)) {

                            final var direction = flatten(clNode.get(Terms.RDF_DIRECTION), Keywords.VALUE);

                            if (!(direction instanceof String dirString)
                                    || (!"ltr".equalsIgnoreCase(dirString)
                                            && !"rtl".equalsIgnoreCase(dirString))) {
                                throw new JsonLdException(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                            }

                            clObject.put(Keywords.DIRECTION, direction);
                        }

                        clArray.add(clObject);
                    }
                    graphMap.set(
                            clEntry.graphName(),
                            clEntry.subject(),
                            clEntry.property(),
                            clArray);
                }
            }

            // 6.2.
            if (!graphMap.contains(graphName, Terms.RDF_NIL)) {
                continue;
            }

            // 6.4.
            for (var usage : graphMap.getUsages(graphName, Terms.RDF_NIL)) {

                // 6.4.1.
                var node = graphMap
                        .get(usage.graphName(), usage.subject())
                        .orElse(Map.of());

                // 6.4.2.
                final var list = new ArrayList<Object>();
                final List<String> listNodes = new ArrayList<>();

                String nodeId = ((String) node.get(Keywords.ID));

                // 6.4.3.
                while (Terms.RDF_REST.equals(usage.property())
                        && BlankNode.isWellFormed(nodeId)
                        && referenceOnce.get(nodeId) != null
                        && node.containsKey(Terms.RDF_FIRST)
                        && node.containsKey(Terms.RDF_REST)
                        && ((Collection<?>) node.get(Terms.RDF_FIRST)).size() == 1
                        && ((Collection<?>) node.get(Terms.RDF_REST)).size() == 1
                        && (node.size() == 3 /* keywords: @id, @first, @last */
                                || (node.size() == 4 && node.containsKey(Keywords.TYPE)
                                        && ((Collection<?>) node.get(Keywords.TYPE)).size() == 1
                                        && ((Collection<?>) node.get(Keywords.TYPE)).contains(Terms.RDF_LIST)))) {

                    // 6.4.3.1.
                    // reverse order -> index = 0 see 6.4.5.
                    list.add(0, ((Collection<?>) node.get(Terms.RDF_FIRST)).iterator().next());

                    // 6.4.3.2.
                    listNodes.add(nodeId);

                    // 6.4.3.3.
                    usage = referenceOnce.get(nodeId);

                    // 6.4.3.4.
                    final var nextNode = graphMap.get(usage.graphName(), usage.subject());

                    if (!nextNode.isPresent()) {
                        break;
                    }

                    node = nextNode.get();

                    if (!node.containsKey(Keywords.ID)) {
                        break;
                    }

                    nodeId = (String) node.get(Keywords.ID);

                    // 6.4.3.5.
                    if (UriUtils.isAbsoluteUri(nodeId, uriValidation)) {
                        break;
                    }
                }

                final var head = usage.value();

                // 6.4.4.
                head.remove(Keywords.ID);

                // 6.4.6.
                head.put(Keywords.LIST, list);

                // 6.4.7.
                listNodes.forEach(nid -> graphMap.remove(graphName, nid));
            }
        }

        // 7.
        final var result = new ArrayList<Object>();

        // 8.
        for (final var subject : Utils.index(graphMap.keys(Keywords.DEFAULT), ordered)) {

            final var node = graphMap.get(Keywords.DEFAULT, subject).orElseGet(() -> new LinkedHashMap<>());

            // 8.1.
            if (graphMap.contains(subject)) {

                final var array = new ArrayList<Object>();

                for (final String key : Utils.index(graphMap.keys(subject), ordered)) {

                    final var entry = graphMap
                            .get(subject, key)
                            .orElse(Map.of());

                    if (entry.size() > 1 || !entry.containsKey(Keywords.ID)) {
                        array.add(Map.copyOf(entry));
                    }
                }

                node.put(Keywords.GRAPH, array);
            }

            // 8.2.
            if (node.size() > 1 || !node.containsKey(Keywords.ID)) {
                result.add(Map.copyOf(node));
            }
        }
        // 9.
        return result;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String object, String datatype, String language, String direction, String graph) throws RdfConsumerException {

        final String graphName = graph == null ? Keywords.DEFAULT : graph;

        if (direction != null || datatype != null && datatype.startsWith(Terms.RDF_I18N_BASE)) {

            datatype = Terms.RDF_I18N_BASE;

            if (language != null) {
                datatype = datatype + language;
                language = null;
            }
            if (direction != null) {
                datatype = datatype + '_' + direction;
                direction = null;
            }

        }

        // 5.3.
        if (!compoundLiteralSubjects.containsKey(graphName)) {
            compoundLiteralSubjects.put(graphName, new LinkedHashMap<>());
        }

        // 5.4.
        if (!Keywords.DEFAULT.equals(graphName) && !graphMap.contains(Keywords.DEFAULT, graphName)) {
            graphMap.set(Keywords.DEFAULT, graphName, Keywords.ID, graphName);
        }

        // 5.6.
        final Map<String, Boolean> compoundMap = compoundLiteralSubjects.get(graphName);

        // 5.7.1.
        if (!graphMap.contains(graphName, subject)) {
            graphMap.set(graphName, subject, Keywords.ID, subject);
        }

        // 5.7.3.
        if (RdfDirection.COMPOUND_LITERAL == rdfDirection
                && Terms.RDF_DIRECTION.equals(predicate)) {

            compoundMap.put(subject, Boolean.TRUE);
        }

        // 5.7.4.
        if (!RdfQuadConsumer.isLiteral(datatype, language, direction)
                && !graphMap.contains(graphName, object)) {

            graphMap.set(graphName, object, Keywords.ID, object);
        }

        // 5.7.5.
        if (!useRdfType && Terms.RDF_TYPE.equals(predicate) && !RdfQuadConsumer.isLiteral(datatype, language, direction)) {

            final var type = graphMap.get(graphName, subject, Keywords.TYPE);

            if (type.isPresent()) {

                var types = new ArrayList<Object>((Collection<?>) type.get());
                types.add(object);

                graphMap.set(graphName, subject, Keywords.TYPE, types);

            } else {

                graphMap.set(graphName, subject, Keywords.TYPE, Set.of(object));
            }

            return this;
        }

        // 5.7.6.
        final var value = toObject(object, datatype, language, direction);

        final var predicateValue = graphMap.get(graphName, subject, predicate);

        // 5.7.7.
        if (predicateValue.isPresent()) {

            var array = (Collection<?>) predicateValue.get();

            if (!array.contains(value)) {
                var list = new ArrayList<Object>(array);
                list.add(value);

                graphMap.set(graphName, subject, predicate, list);
            }

            // 5.7.8.
        } else {
            graphMap.set(graphName, subject, predicate, List.of(value));
        }

        // 5.7.9.
        if (Terms.RDF_NIL.equals(object)) {

            graphMap.addUsage(graphName, object, new Reference(
                    graphName,
                    subject,
                    predicate,
                    value));

            // 5.7.10.
        } else if (referenceOnce.containsKey(object)) {

            referenceOnce.put(object, null);

            // 5.7.11.
        } else if (RdfQuadConsumer.isBlank(object)) {

            referenceOnce.put(object, new Reference(
                    graphName,
                    subject,
                    predicate,
                    value));
        }

        return this;
    }

    Map<String, Object> toObject(String object, String datatype, String langTag, String direction) throws RdfConsumerException {

        // 1.
        if (!RdfQuadConsumer.isLiteral(datatype, langTag, direction)) {
            final var ref = new LinkedHashMap<String, Object>(1);
            ref.put(Keywords.ID, object);
            return ref;
        }

        // 2.2.
        Object convertedValue = null;

        // 2.4.
        if (!nativeTypes.isEmpty()) {

            if (datatype != null) {

                final var convertor = nativeTypes.get(datatype);

                if (convertor != null) {
                    convertedValue = convertor.apply(object);
                    if (convertedValue != null) {
                        return Map.of(Keywords.VALUE, convertedValue);
                    }
                }

                return Map.of(
                        Keywords.VALUE, object,
                        Keywords.TYPE, datatype);
            }
            return Map.of(
                    Keywords.VALUE, object);
        }

        // 2.5.
        if (jsonParser != null
                && processingMode != JsonLdVersion.V1_0
                && Terms.RDF_JSON.equals(datatype)) {

            try {

                convertedValue = jsonParser.parse(new ByteArrayInputStream(object.getBytes()));

                if (convertedValue == null) {
                    final var result = new HashMap<String, Object>(2);
                    result.put(Keywords.TYPE, Keywords.JSON);
                    result.put(Keywords.VALUE, null);
                    return Map.copyOf(result);
                }

                return Map.of(
                        Keywords.VALUE, convertedValue,
                        Keywords.TYPE, Keywords.JSON);

            } catch (Exception e) {
                throw new RdfConsumerException(new JsonLdException(JsonLdErrorCode.INVALID_JSON_LITERAL, e));
            }
        }

        final var result = new LinkedHashMap<String, Object>(5);

        // 2.3.
        String type = null;

        // 2.6.
        if (RdfDirection.I18N_DATATYPE == rdfDirection
                && datatype != null
                && datatype.startsWith(Terms.RDF_I18N_BASE)) {

            convertedValue = object;

            String dirLang = datatype.substring(Terms.RDF_I18N_BASE.length());

            int directionIndex = dirLang.indexOf('_');

            if (directionIndex > 1) {

                result.put(Keywords.LANGUAGE, dirLang.substring(0, directionIndex));
                result.put(Keywords.DIRECTION, dirLang.substring(directionIndex + 1));

            } else if (directionIndex == 0) {

                result.put(Keywords.DIRECTION, dirLang.substring(1));

            } else if (directionIndex == -1) {

                result.put(Keywords.LANGUAGE, dirLang);
            }

            // 2.7.
        } else if (langTag != null) {

            result.put(Keywords.LANGUAGE, langTag);

            // 2.8.
        } else if (datatype != null
                && !Terms.XSD_STRING.equals(datatype)) {

            type = datatype;
        }

        // 2.9.
        result.put(Keywords.VALUE,
                (convertedValue != null)
                        ? convertedValue
                        : object);

        // 2.10.
        if (type != null) {
            result.put(Keywords.TYPE, type);
        }

        // 2.11.
        return result;
    }

    public static Boolean toBoolean(String value) {

        if ("true".equals(value) || "1".equals(value)) {
            return true;

        } else if ("false".equals(value) || "0".equals(value)) {
            return false;
        }
        return null;
    }

    public static Long toLong(String value) {

        try {

            return Long.parseLong(value);

        } catch (NumberFormatException e) {

        }
        return null;
    }

    public static Double toDouble(String value) {
        try {
            final Double number = Double.parseDouble(value);

            if (!number.isInfinite() && !number.isNaN()) {
                return number;
            }

        } catch (NumberFormatException e) {
        }
        return null;
    }

    public static BigDecimal toDecimal(String value) {

        try {
            return new BigDecimal(value);

        } catch (NumberFormatException e) {
        }
        return null;
    }

    static Object flatten(Object value, String key) {

        var result = value;

        if (result instanceof Collection array && array.size() == 1) {
            result = array.iterator().next();
        }

        if (result instanceof Map map) {
            result = map.get(key);
        }

        return result;
    }

}