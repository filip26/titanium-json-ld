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
package com.apicatalog.jsonld.tordf;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.Options.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.web.lang.LanguageTag;
import com.apicatalog.web.uri.BlankNode;
import com.apicatalog.web.uri.UriUtils;
import com.apicatalog.web.uri.UriValidationPolicy;

public final class JsonLdToQuads {

    @FunctionalInterface
    public interface QuadConsumer {

        /**
         * Consumes an RDF quad where the {@code object} may be an IRI, blank node,
         * typed literal, or language-tagged literal.
         * <p>
         * This method provides fine-grained control over RDF quad data, allowing
         * precise handling of datatypes and language tags.
         *
         * @param subject   the subject of the quad; must be an IRI or blank node
         *                  identifier prefixed with "<code>_:</code>". Must not be
         *                  {@code null}.
         * @param predicate the predicate of the quad; must be an IRI. Must not be
         *                  {@code null}.
         * @param object    the object of the quad; must be either:
         *                  <ul>
         *                  <li>an IRI</li>
         *                  <li>a blank node identifier prefixed with
         *                  "<code>_:</code>"</li>
         *                  <li>a literal value, when {@code datatype} is not
         *                  {@code null}</li>
         *                  </ul>
         *                  Must not be {@code null}.
         *                  <p>
         *                  Use {@link #isLiteral(String, String, String)},
         *                  {@link #isLangString(String, String, String)},
         *                  {@link #isDirLangString(String, String, String)}, and
         *                  {@link #isI18nString(String, String, String)} to validate
         *                  and classify the input.
         * @param datatype  the datatype IRI of the literal. Must be {@code null} if
         *                  {@code object} is not a literal. Must not be {@code null}
         *                  when {@code language} is provided.
         * @param language  the language tag of the literal. May be {@code null}.
         * @param direction the text direction, or null
         * @param graph     the graph name of the quad; must be an IRI or blank node
         *                  identifier prefixed with "<code>_:</code>". May be
         *                  {@code null} to indicate the default graph.
         *
         * @throws IllegalArgumentException if an error occurs while processing the quad
         *                                  statement.
         */
        void quad(
                String subject,
                String predicate,
                String object,
                String datatype,
                String language,
                String direction,
                String graph);
    }

    @FunctionalInterface
    public interface JsonLiteralWriter {

        String write(Object node) throws JsonLdException;
    }

    private static final Logger LOGGER = Logger.getLogger(JsonLdToQuads.class.getName());

    private static final DecimalFormat xsdDecimalFormat = new DecimalFormat("0.0##############E0",
            new DecimalFormatSymbols(Locale.ENGLISH));

    static {
        xsdDecimalFormat.setMinimumFractionDigits(1);
    }

    // required
    private final NodeMap nodeMap;
    private final JsonLiteralWriter jsonWriter;

    // optional
    private boolean produceGeneralizedRdf;
    private RdfDirection rdfDirection;
    private UriValidationPolicy uriValidation;

    private JsonLdToQuads(NodeMap nodeMap, JsonLiteralWriter jsonWriter) {
        this.nodeMap = nodeMap;

        this.produceGeneralizedRdf = false;
        this.jsonWriter = null;
        this.rdfDirection = null;
        this.uriValidation = Options.DEFAULT_URI_VALIDATION;
    }

    public static final JsonLdToQuads with(NodeMap nodeMap, JsonLiteralWriter jsonWriter) {
        return new JsonLdToQuads(nodeMap, jsonWriter);
    }

    public void provide(QuadConsumer consumer) throws JsonLdException {
        provide(RdfQuadEmitter.newEmitter(consumer));
    }

    public JsonLdToQuads produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }

    public JsonLdToQuads rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public JsonLdToQuads uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    protected void provide(RdfTripleConsumer consumer) throws JsonLdException {

        final var graphNames = nodeMap.graphs()
                .stream()
                .sorted()
                .iterator();

        while (graphNames.hasNext()) {

            final var graphName = graphNames.next();

            if (Keywords.DEFAULT.equals(graphName)) {
                consumer.defaultGraph();

            } else if (BlankNode.isWellFormed(graphName)) {
                consumer.namedGraph(graphName);

            } else if (UriUtils.isAbsoluteUri(graphName, uriValidation)) {
                consumer.namedGraph(graphName);

            } else {
                LOGGER.log(Level.WARNING, "Graph has been skipped because of non well-formed graph name [{0}]",
                        graphName);
                continue;
            }

            final var subjects = nodeMap.subjects(graphName)
                    .stream()
                    .sorted()
                    .iterator();

            while (subjects.hasNext()) {

                final var subject = subjects.next();

                if (!BlankNode.isWellFormed(subject) && UriUtils.isNotAbsoluteUri(subject, uriValidation)) {
                    LOGGER.log(Level.WARNING, "Non well-formed subject [{0}] has been skipped.", subject);
                    continue;
                }

                final var properties = nodeMap.properties(graphName, subject)
                        .stream()
                        .sorted()
                        .iterator();

                while (properties.hasNext()) {

                    final var property = properties.next();

                    if (Keywords.TYPE.equals(property)) {

                        for (final var type : (Collection<?>) nodeMap.get(graphName, subject, property)) {

                            if (type instanceof String typeString) {

                                if (!BlankNode.isWellFormed(typeString)
                                        && UriUtils.isNotAbsoluteUri(typeString, uriValidation)) {
                                    LOGGER.log(Level.WARNING, "Non well-formed type [{0}] has been skipped.",
                                            typeString);
                                    continue;
                                }

                                consumer.triple(
                                        subject,
                                        Terms.RDF_TYPE,
                                        typeString);

                            } else {
                                LOGGER.log(Level.WARNING, "Non well-formed type [{0}] has been skipped.", type);
                                continue;
                            }

                        }

                    } else if (!Keywords.contains(property)) {

                        if ((!BlankNode.isWellFormed(property)
                                || produceGeneralizedRdf)
                                && UriUtils.isNotAbsoluteUri(property, uriValidation)) {
                            continue;
                        }

                        @SuppressWarnings("unchecked")
                        final var items = (Collection<Map<String, Object>>) nodeMap.get(graphName, subject, property);

                        for (final var item : items) {
                            fromObject(
                                    consumer,
                                    item,
                                    subject,
                                    property);
                        }
                    }
                }
            }
        }
    }

    /*
     * @see <a href=
     * "https://w3c.github.io/json-ld-api/#deserialize-json-ld-to-rdf-algorithm">
     * Object to RDF Conversion</a>
     */
    private void fromObject(
            final RdfTripleConsumer consumer,
            final Map<String, ?> item,
            final String subject,
            final String predicate) throws JsonLdException {

        // 1. - 2.
        if (JsonLdAdapter.isNode(item)) {

            if (item.get(Keywords.ID) instanceof String nodeId
                    && (BlankNode.isWellFormed(nodeId)
                            || UriUtils.isAbsoluteUri(nodeId, uriValidation))) {

                consumer.triple(subject, predicate, nodeId);
            }
            return;
        }

        // 3.
        if (JsonLdAdapter.isList(item)) {

            @SuppressWarnings("unchecked")
            final var list = (Collection<Map<String, Object>>) item.get(Keywords.LIST);

            fromList(consumer, list, subject, predicate);
        }

        // 4.
        if (!JsonLdAdapter.isValueNode(item)) {
            return;
        }

        final var value = item.get(Keywords.VALUE);

        // 5.
        var datatype = item.get(Keywords.TYPE) instanceof String type
                ? type
                : null;

        // 6.
        if (datatype != null
                && !Keywords.JSON.equals(datatype)
                && !UriUtils.isAbsoluteUri(datatype, uriValidation)) {
            LOGGER.log(Level.WARNING, "Datatype [{0}] is not an absolute URI nor @json and value is skipped.",
                    datatype);
            return;
        }

        // 7.
        if (item.containsKey(Keywords.LANGUAGE)
                && (!(item.get(Keywords.LANGUAGE) instanceof String langString)
                        || !LanguageTag.isWellFormed(langString))) {
            LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed string and value is skipped.",
                    item.get(Keywords.LANGUAGE));
            return;
        }

        String valueString = null;

        // 8.
        if (Keywords.JSON.equals(datatype)) {
            valueString = jsonWriter.write(value);
            datatype = Terms.RDF_JSON;

            // 9.
        } else if (Boolean.TRUE.equals(value)) {
            valueString = "true";

            if (datatype == null) {
                datatype = Terms.XSD_BOOLEAN;
            }

        } else if (Boolean.FALSE.equals(value)) {
            valueString = "false";

            if (datatype == null) {
                datatype = Terms.XSD_BOOLEAN;
            }

            // 10. - 11.
        } else if (value instanceof Number number) {

            if (Terms.XSD_DOUBLE.equals(datatype)
                    || Terms.XSD_FLOAT.equals(datatype)
//                    || (!JavaAdapter.instance().isIntegral(number) && number.doubleValue() % -1 != 0)
                    || (number instanceof BigDecimal decimal
                            && decimal.compareTo(BigDecimal.ONE.movePointRight(21)) >= 0)
                    || (number instanceof Double decimal && decimal >= 1e21)
                    || (number instanceof Float decimal && decimal >= 1e21f)) {

                valueString = toXsdDouble(number.doubleValue());

                if (datatype == null) {
                    datatype = Terms.XSD_DOUBLE;
                }

            } else {
                valueString = Long.toString(number.longValue());

                if (datatype == null) {
                    datatype = Terms.XSD_INTEGER;
                }

            }

            // 12.
        } else if (datatype == null) {

            datatype = item.containsKey(Keywords.LANGUAGE)
                    ? Terms.RDF_LANG_STRING
                    : Terms.XSD_STRING;
        }

        if (valueString == null) {

            if (value instanceof String valueAsString) {

                valueString = valueAsString;

            } else {
                return;
            }
        }

        // 13.
        if (item.containsKey(Keywords.DIRECTION) && rdfDirection != null) {

            // 13.2.
            if (RdfDirection.I18N_DATATYPE == rdfDirection) {

                final var language = item.get(Keywords.LANGUAGE) instanceof String langString
                        ? langString.toLowerCase()
                        : "";

                consumer.triple(
                        subject,
                        predicate,
                        valueString,
                        language,
                        (String) item.get(Keywords.DIRECTION));

                // 13.3.
            } else if (RdfDirection.COMPOUND_LITERAL == rdfDirection) {

                final String blankNodeId = nodeMap.createIdentifier();

                // 13.3.2.
                consumer.triple(
                        blankNodeId,
                        Terms.RDF_VALUE,
                        valueString,
                        Terms.XSD_STRING);

                // 13.3.3.
                if (item.get(Keywords.LANGUAGE) instanceof String langString) {
                    consumer.triple(
                            blankNodeId,
                            Terms.RDF_LANGUAGE,
                            langString.toLowerCase(),
                            Terms.XSD_STRING);
                }

                // 13.3.4.
                consumer.triple(
                        blankNodeId,
                        Terms.RDF_DIRECTION,
                        (String) item.get(Keywords.DIRECTION),
                        Terms.XSD_STRING);

                consumer.triple(subject, predicate, blankNodeId);
                return;
            }

            // 14.
        } else {
            if (item.get(Keywords.LANGUAGE) instanceof String langString) {
                consumer.triple(
                        subject,
                        predicate,
                        valueString,
                        langString,
                        null);

            } else {
                consumer.triple(
                        subject,
                        predicate,
                        valueString,
                        datatype);
            }
        }
    }

    /*
     * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List
     * to RDF Conversion</a>
     */
    private void fromList(
            final RdfTripleConsumer consumer,
            final Collection<Map<String, Object>> list,
            final String subject,
            final String predicate) throws JsonLdException {

        // 1.
        if (list.isEmpty()) {
            consumer.triple(subject, predicate, Terms.RDF_NIL);
            return;
        }

        // 2.
        final var bnodes = new String[list.size()];

        for (int i = 0; i < bnodes.length; i++) {
            bnodes[i] = nodeMap.createIdentifier();
        }

        consumer.triple(subject, predicate, bnodes[0]);

        // 3.
        int index = 0;
        for (final var item : list) {

            final String blankNodeSubject = bnodes[index];
            index++;

            fromObject(
                    consumer,
                    item,
                    blankNodeSubject,
                    Terms.RDF_FIRST);

            // 3.4.
            if (index < bnodes.length) {
                consumer.triple(blankNodeSubject, Terms.RDF_REST, bnodes[index]);

            } else {
                consumer.triple(blankNodeSubject, Terms.RDF_REST, Terms.RDF_NIL);
            }
        }
    }

    private static final String toXsdDouble(double number) {
        return xsdDecimalFormat.format(number);
    }
}
