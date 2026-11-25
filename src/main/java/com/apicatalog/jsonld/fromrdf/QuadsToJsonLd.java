/*
 * Copyright 2020-2026 the original author or authors.
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

import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.Options.RdfDirection;
import com.apicatalog.jsonld.fromrdf.GraphMap.Reference;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.web.lang.LanguageTag;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.BlankNode;
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
 * {@link #useRdfType(boolean)}, {@link #mode(Version)}, and
 * {@link #uriValidation(UriValidationPolicy)}.
 * </p>
 * <p>
 * Key methods include:
 * </p>
 * <ul>
 * <li>{@link #reset()} to clear the internal state for reuse.</li>
 * <li>{@link #toJsonLd()} to generate the JSON-LD output from the processed
 * quads.</li>
 * <li>{@link #quad(String, String, String, String, String, String, String)} to
 * ingest a single RDF quad.</li>
 * </ul>
 * 
 * @since 1.7.0
 */
public final class QuadsToJsonLd {

    @FunctionalInterface
    public interface JsonLiteralReader {
        Object read(MediaType contentType, Reader reader);
    }

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

    protected UriValidationPolicy uriValidation;
    protected Version processingMode;

    protected JsonLiteralReader jsonReader;
    protected Map<String, Function<String, Object>> nativeTypes;

    // runtime
    protected GraphMap graphMap;
    protected Map<String, Map<String, Boolean>> compoundLiteralSubjects;
    protected Map<String, Reference> referenceOnce;
    protected JsonLdException error;

    public QuadsToJsonLd() {
        this.graphMap = new GraphMap();
        this.referenceOnce = new LinkedHashMap<>();
        this.compoundLiteralSubjects = new LinkedHashMap<>();

        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.jsonReader = null;
        this.nativeTypes = Map.of();
        this.useRdfType = false;
        this.uriValidation = Options.DEFAULT_URI_VALIDATION;
        this.processingMode = Version.V1_1;
        this.error = null;
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
    public QuadsToJsonLd mode(Version processingMode) {
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

    public QuadsToJsonLd jsonReader(JsonLiteralReader jsonReader) {
        this.jsonReader = jsonReader;
        return this;
    }

    public QuadsToJsonLd options(Options options) {
        this.ordered = options.isOrdered();
        this.rdfDirection = options.rdfDirection();
        this.useNativeTypes(options.useNativeTypes());
        this.useRdfType = options.useRdfType();
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

                for (final var cl : compoundLiteralSubjects.get(graphName).keySet()) {

                    // 6.1.1.
                    final var clEntry = referenceOnce.get(cl);

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
                            .map(JsonLdAdapter::asCollection)
                            .orElse(List.of())) {

                        if (clReference == null || !(clReference instanceof Map)) {
                            continue;
                        }

                        @SuppressWarnings({ "unchecked" })
                        final var clReferenceObject = (Map<String, Object>) clReference;

                        if (!clReferenceObject.containsKey(Keywords.ID)
                                || !cl.equals(clReferenceObject.get(Keywords.ID))) {
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
                                throw new JsonLdException(ErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                            }

                            clObject.put(Keywords.LANGUAGE, lang);
                        }

                        // 6.1.6.4.
                        if (clNode.containsKey(Terms.RDF_DIRECTION)) {

                            final var direction = flatten(clNode.get(Terms.RDF_DIRECTION), Keywords.VALUE);

                            if (!(direction instanceof String dirString)
                                    || (!"ltr".equalsIgnoreCase(dirString)
                                            && !"rtl".equalsIgnoreCase(dirString))) {
                                throw new JsonLdException(ErrorCode.INVALID_BASE_DIRECTION);
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
                final var list = new ArrayList<>();
                final var listNodes = new ArrayList<String>();

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

        final Collection<String> subjects;

        if (ordered) {
            var sorted = new ArrayList<>(graphMap.keys(Keywords.DEFAULT));
            Collections.sort(sorted);
            subjects = sorted;

        } else {
            subjects = graphMap.keys(Keywords.DEFAULT);
        }

        // 8.
        for (final var subject : subjects) {

            final var node = graphMap
                    .get(Keywords.DEFAULT, subject)
                    .orElseGet(() -> new LinkedHashMap<>());

            // 8.1.
            if (graphMap.contains(subject)) {

                final var array = new ArrayList<Object>();

                final var keys = ordered
                        ? graphMap.keys(subject).stream().sorted().iterator()
                        : graphMap.keys(subject).iterator();

                while (keys.hasNext()) {

                    final var entry = graphMap
                            .get(subject, keys.next())
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

    public void quad(
            final String subject,
            final String predicate,
            final String object,
            final String datatype,
            final String language,
            final String direction,
            final String graph) {

        final var graphName = graph == null ? Keywords.DEFAULT : graph;

        // 5.3.
        if (!compoundLiteralSubjects.containsKey(graphName)) {
            compoundLiteralSubjects.put(graphName, new LinkedHashMap<>());
        }

        // 5.4.
        if (!Keywords.DEFAULT.equals(graphName) && !graphMap.contains(Keywords.DEFAULT, graphName)) {
            graphMap.set(Keywords.DEFAULT, graphName, Keywords.ID, graphName);
        }

        // 5.7.1.
        if (!graphMap.contains(graphName, subject)) {
            graphMap.set(graphName, subject, Keywords.ID, subject);
        }

        // 5.7.3.
        if (rdfDirection == RdfDirection.COMPOUND_LITERAL && Terms.RDF_DIRECTION.equals(predicate)) {
            compoundLiteralSubjects.get(graphName).put(subject, Boolean.TRUE);
        }

        final var isObjectResource = isObject(datatype, language, direction);

        // 5.7.4.
        if (isObjectResource && !graphMap.contains(graphName, object)) {
            graphMap.set(graphName, object, Keywords.ID, object);
        }

        // 5.7.5.
        if (!useRdfType && isObjectResource && Terms.RDF_TYPE.equals(predicate)) {

            final var type = graphMap.get(graphName, subject, Keywords.TYPE);

            if (type.isPresent()) {
                var types = new ArrayList<Object>((Collection<?>) type.get());
                types.add(object);

                graphMap.set(graphName, subject, Keywords.TYPE, types);
                return;
            }

            graphMap.set(graphName, subject, Keywords.TYPE, Set.of(object));
            return;
        }

        // 5.7.6.
        final Map<String, Object> value;

        // 1.
        if (isObjectResource) {
            final var ref = new LinkedHashMap<String, Object>(1);
            ref.put(Keywords.ID, object);
            value = ref;

        } else {
            value = toObject(object, datatype, language, direction);
        }

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
        } else if (isBlank(object)) {

            referenceOnce.put(object, new Reference(
                    graphName,
                    subject,
                    predicate,
                    value));
        }

    }

    public JsonLdException error() {
        return error;
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

    private Map<String, Object> toObject(
            String object,
            String datatype,
            String language,
            String direction) {

        // 2.2.
        Object convertedValue = null;

        // 2.4.
        if (nativeTypes.containsKey(datatype)) {

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
            return Map.of(Keywords.VALUE, object);
        }

        // 2.5.
        if (jsonReader != null
                && processingMode != Version.V1_0
                && Terms.RDF_JSON.equals(datatype)) {

            try {

                convertedValue = jsonReader.read(MediaType.JSON, new StringReader(object));

                if (convertedValue == null) {
                    final var result = HashMap.<String, Object>newHashMap(2);
                    result.put(Keywords.TYPE, Keywords.JSON);
                    result.put(Keywords.VALUE, null);
                    return result;
                }

                return Map.of(
                        Keywords.VALUE, convertedValue,
                        Keywords.TYPE, Keywords.JSON);

            } catch (Exception e) {
                this.error = new JsonLdException(ErrorCode.INVALID_JSON_LITERAL, e);
                throw e;
            }
        }

        String valueType = datatype;
        String valueLanguage = language;
        String valueDirection = direction;

        // 2.6.
        if (datatype != null && datatype.startsWith(Terms.RDF_I18N_BASE)) {

            if (rdfDirection == RdfDirection.I18N_DATATYPE
                    && datatype.length() == Terms.RDF_I18N_BASE.length()) {
                valueType = null;

            } else if (rdfDirection != RdfDirection.I18N_DATATYPE
                    && datatype.length() == Terms.RDF_I18N_BASE.length()) {

                if (language != null) {
                    valueType = datatype + language;
                    valueLanguage = null;
                }
                if (direction != null) {
                    valueType = valueType + '_' + direction;
                    valueDirection = null;
                }

            } else if (rdfDirection == RdfDirection.I18N_DATATYPE
                    && datatype.length() > (Terms.RDF_I18N_BASE.length() + 1)) {

                convertedValue = object;

                final var dirLang = datatype.substring(Terms.RDF_I18N_BASE.length());

                final var directionIndex = dirLang.indexOf('_');

                if (directionIndex > 1) {
                    valueLanguage = dirLang.substring(0, directionIndex);
                    valueDirection = dirLang.substring(directionIndex + 1);
                    valueType = Terms.RDF_I18N_BASE;

                } else if (directionIndex == 0) {
                    valueDirection = dirLang.substring(1);
                    valueType = Terms.RDF_I18N_BASE;
                }
            }
        }

        final var result = HashMap.<String, Object>newHashMap(4);

        // 2.7.
        if (valueLanguage != null && !(Terms.RDF_DIR_LANG_STRING.equals(valueType) && "und".equals(language))) {
            result.put(Keywords.LANGUAGE, valueLanguage);
        }

        if (valueDirection != null) {
            result.put(Keywords.DIRECTION, valueDirection);
        }

        if (valueType != null
                && !Terms.XSD_STRING.equals(valueType)
                && !Terms.RDF_LANG_STRING.equals(valueType)
                && !Terms.RDF_DIR_LANG_STRING.equals(valueType)) {
            result.put(Keywords.TYPE, valueType);
        }

        // 2.9.
        result.put(Keywords.VALUE,
                (convertedValue != null)
                        ? convertedValue
                        : object);

        // 2.11.
        return result;
    }

    private static Object flatten(Object value, String key) {

        var result = value;

        if (result instanceof Collection array && array.size() == 1) {
            result = array.iterator().next();
        }

        if (result instanceof Map map) {
            result = map.get(key);
        }

        return result;
    }

    private static boolean isObject(String datatype, String language, String direction) {
        return datatype == null && language == null && direction == null;
    }

    /**
     * Checks whether the provided resource identifier represents a blank node. A
     * blank node identifier must start with "<code>_:</code>".
     *
     * @param resource the resource identifier to check; may be {@code null}.
     * @return {@code true} if the resource is a non-null blank node identifier;
     *         otherwise {@code false}.
     */
    private static boolean isBlank(String resource) {
        return resource != null && resource.startsWith("_:");
    }
}