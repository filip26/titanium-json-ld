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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonCanonicalizer;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfConsumer;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfDatasetSupplier;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class JsonLdToRdf {

    private static final Logger LOGGER = Logger.getLogger(JsonLdToRdf.class.getName());

    private static final DecimalFormat xsdNumberFormat = new DecimalFormat("0.0##############E0", new DecimalFormatSymbols(Locale.ENGLISH));

    static {
        xsdNumberFormat.setMinimumFractionDigits(1);
    }

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

    /**
     * @deprecated since 1.6.0, use {@link #with(NodeMap)} and
     *             {@link #provide(RdfConsumer)}.
     * @param nodeMap
     * @param dataset
     * @return
     */
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

    public void provide(RdfConsumer consumer) throws JsonLdError {

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
                            fromObject(
                                    consumer,
                                    item.asJsonObject(),
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
     * @deprecated since 1.6.0, use {@link #provide(RdfConsumer)}.
     * @return
     * @throws JsonLdError
     */
    @Deprecated
    public RdfDataset build() throws JsonLdError {

        if (dataset == null) {
            dataset = Rdf.createDataset();
        }

        provide(new RdfDatasetSupplier(dataset));

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

    /*
     * @see <a href=
     * "https://w3c.github.io/json-ld-api/#deserialize-json-ld-to-rdf-algorithm">
     * Object to RDF Conversion</a>
     */
    private void fromObject(
            final RdfConsumer consumer,
            final JsonObject item,
            final String subject,
            final boolean blankSubject,
            final String predicate,
            final boolean blankPredicate) throws JsonLdError {

        // 1. - 2.
        if (NodeObject.isNodeObject(item)) {

            JsonValue id = item.get(Keywords.ID);

            if (JsonUtils.isNotString(id) || JsonUtils.isNull(id)) {
                return;
            }

            String idString = ((JsonString) id).getString();

            if (BlankNode.isWellFormed(idString)) {
                consumer.accept(subject, blankSubject, predicate, blankPredicate, idString, true);

            } else if (UriUtils.isAbsoluteUri(idString, uriValidation)) {
                consumer.accept(subject, blankSubject, predicate, blankPredicate, idString, false);
            }

            return;
        }

        // 3.
        if (ListObject.isListObject(item)) {
            fromList(consumer, item.get(Keywords.LIST).asJsonArray(), subject, blankSubject, predicate, blankPredicate);
        }

        // 4.
        if (!ValueObject.isValueObject(item)) {
            return;
        }

        final JsonValue value = item.get(Keywords.VALUE);

        // 5.
        String datatype = item.containsKey(Keywords.TYPE) && JsonUtils.isString(item.get(Keywords.TYPE))
                ? item.getString(Keywords.TYPE)
                : null;

        // 6.
        if (datatype != null && !Keywords.JSON.equals(datatype) && !UriUtils.isAbsoluteUri(datatype, uriValidation)) {
            LOGGER.log(Level.WARNING, "Datatype [{0}] is not an absolute IRI nor @json and value is skipped.", datatype);
            return;
        }

        // 7.
        if (item.containsKey(Keywords.LANGUAGE)
                && (JsonUtils.isNotString(item.get(Keywords.LANGUAGE))
                        || !LanguageTag.isWellFormed(item.getString(Keywords.LANGUAGE)))) {
            LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed string and value is skipped.", item.get(Keywords.LANGUAGE));
            return;
        }

        String valueString = null;

        // 8.
        if (Keywords.JSON.equals(datatype)) {
            valueString = JsonCanonicalizer.canonicalize(value);
            datatype = RdfConstants.JSON;

            // 9.
        } else if (JsonUtils.isTrue(value)) {

            valueString = "true";

            if (datatype == null) {
                datatype = XsdConstants.BOOLEAN;
            }

        } else if (JsonUtils.isFalse(value)) {

            valueString = "false";

            if (datatype == null) {
                datatype = XsdConstants.BOOLEAN;
            }

            // 10. - 11.
        } else if (JsonUtils.isNumber(value)) {

            JsonNumber number = ((JsonNumber) value);

            // 11.
            if ((!number.isIntegral() && number.doubleValue() % -1 != 0)
                    || XsdConstants.DOUBLE.equals(datatype)
                    || XsdConstants.FLOAT.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

                valueString = toXsdDouble(number.bigDecimalValue());

                if (datatype == null) {
                    datatype = XsdConstants.DOUBLE;
                }

                // 10.
            } else {

                valueString = number.bigIntegerValue().toString();

                if (datatype == null) {
                    datatype = XsdConstants.INTEGER;
                }

            }

            // 12.
        } else if (datatype == null) {

            datatype = item.containsKey(Keywords.LANGUAGE)
                    ? RdfConstants.LANG_STRING
                    : XsdConstants.STRING;
        }

        if (valueString == null) {

            if (JsonUtils.isNotString(value)) {
                return;
            }

            valueString = ((JsonString) value).getString();
        }

        // 13.
        if (item.containsKey(Keywords.DIRECTION) && rdfDirection != null) {

            // 13.1.
            final String language = item.containsKey(Keywords.LANGUAGE)
                    ? item.getString(Keywords.LANGUAGE).toLowerCase()
                    : "";
            // 13.2.
            if (RdfDirection.I18N_DATATYPE == rdfDirection) {
                datatype = "https://www.w3.org/ns/i18n#"
                        .concat(language)
                        .concat("_")
                        .concat(item.getString(Keywords.DIRECTION));

                consumer.accept(
                        subject, blankSubject,
                        predicate, blankPredicate,
                        valueString, datatype, null);

                // 13.3.
            } else if (RdfDirection.COMPOUND_LITERAL == rdfDirection) {

                final String blankNodeId = nodeMap.createIdentifier();

                // 13.3.2.
                consumer.accept(
                        blankNodeId,
                        true,
                        RdfConstants.VALUE,
                        false,
                        valueString,
                        XsdConstants.STRING,
                        null);

                // 13.3.3.
                if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {
                    consumer.accept(
                            blankNodeId,
                            true,
                            RdfConstants.LANGUAGE,
                            false,
                            item.getString(Keywords.LANGUAGE).toLowerCase(),
                            XsdConstants.STRING,
                            null);
                }

                // 13.3.4.
                consumer.accept(
                        blankNodeId,
                        true,
                        RdfConstants.DIRECTION,
                        false,
                        item.getString(Keywords.DIRECTION),
                        XsdConstants.STRING,
                        null);

                consumer.accept(subject, blankSubject, predicate, blankPredicate, blankNodeId, true);
                return;
            }

            // 14.
        } else {
            if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {
                consumer.accept(
                        subject, blankSubject,
                        predicate, blankPredicate,
                        valueString, RdfConstants.LANG_STRING, item.getString(Keywords.LANGUAGE));

            } else {
                consumer.accept(
                        subject, blankSubject,
                        predicate, blankPredicate,
                        valueString, datatype, null);
            }
        }
    }

    /*
     * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List
     * to RDF Conversion</a>
     */
    private void fromList(
            final RdfConsumer consumer,
            final JsonArray list,
            final String subject,
            final boolean blankSubject,
            final String predicate,
            final boolean blankPredicate) throws JsonLdError {

        // 1.
        if (JsonUtils.isEmptyArray(list)) {
            consumer.accept(subject, blankSubject, predicate, blankPredicate, RdfConstants.NIL, false);
            return;
        }

        // 2.
        final String[] bnodes = new String[list.size()];

        IntStream.range(0, bnodes.length).forEach(i -> bnodes[i] = nodeMap.createIdentifier());

        consumer.accept(subject, blankSubject, predicate, blankPredicate, bnodes[0], true);

        // 3.
        int index = 0;
        for (final JsonValue item : list) {

            final String blankNodeSubject = bnodes[index];
            index++;

            fromObject(
                    consumer,
                    item.asJsonObject(),
                    blankNodeSubject,
                    true,
                    RdfConstants.FIRST,
                    false);

            // 3.4.
            if (index < bnodes.length) {
                consumer.accept(blankNodeSubject, true, RdfConstants.REST, false, bnodes[index], true);

            } else {
                consumer.accept(blankNodeSubject, true, RdfConstants.REST, false, RdfConstants.NIL, false);
            }
        }
    }

    private static final String toXsdDouble(BigDecimal bigDecimal) {
        return xsdNumberFormat.format(bigDecimal);
    }
}
