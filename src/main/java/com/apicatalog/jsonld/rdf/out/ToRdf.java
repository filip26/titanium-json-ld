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
package com.apicatalog.jsonld.rdf.out;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

import com.apicatalog.jcs.Jcs;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.RdfConstants;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.lang.XsdConstants;
import com.apicatalog.jsonld.node.BlankNode;
import com.apicatalog.jsonld.node.ListNode;
import com.apicatalog.jsonld.node.NodeObject;
import com.apicatalog.jsonld.node.ValueNode;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class ToRdf {

    private static final Logger LOGGER = Logger.getLogger(ToRdf.class.getName());

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

    private ToRdf(NodeMap nodeMap) {
        this.nodeMap = nodeMap;

        this.produceGeneralizedRdf = false;
        this.rdfDirection = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static final ToRdf with(NodeMap nodeMap) {
        return new ToRdf(nodeMap);
    }

    public ToRdf produceGeneralizedRdf(boolean enable) {
        this.produceGeneralizedRdf = enable;
        return this;
    }

    public ToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public void provide(RdfQuadConsumer consumer) throws JsonLdError {
        try {
            from(RdfQuadEmitter.newInstance(consumer));
        } catch (RdfConsumerException e) {
            if (e.getCause() instanceof JsonLdError) {
                throw (JsonLdError)e.getCause();
            }
            throw new JsonLdError(JsonLdErrorCode.UNSPECIFIED, e);
        }
    }
    
    protected void from(RdfTripleConsumer consumer) throws JsonLdError, RdfConsumerException {

        for (final String graphName : Utils.index(nodeMap.graphs(), true)) {

            if (Keywords.DEFAULT.equals(graphName)) {
                consumer.defaultGraph();

            } else if (BlankNode.isWellFormed(graphName)) {
                consumer.namedGraph(graphName);

            } else if (UriUtils.isAbsoluteUri(graphName, uriValidation)) {
                consumer.namedGraph(graphName);

            } else {
                //TODO log skipped graph
                continue;
            }

            for (final String subject : Utils.index(nodeMap.subjects(graphName), true)) {

                if (!BlankNode.isWellFormed(subject) && UriUtils.isNotAbsoluteUri(subject, uriValidation)) {
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

                            if (!BlankNode.isWellFormed(typeString) && UriUtils.isNotAbsoluteUri(typeString, uriValidation)) {
                                continue;
                            }

                            consumer.triple(
                                    subject,
                                    RdfConstants.TYPE,
                                    typeString);
                        }

                    } else if (!Keywords.contains(property)) {

                        if ((!BlankNode.isWellFormed(property) || produceGeneralizedRdf) && UriUtils.isNotAbsoluteUri(property, uriValidation)) {
                            continue;
                        }

                        for (final JsonValue item : nodeMap.get(graphName, subject, property).asJsonArray()) {
                            fromObject(
                                    consumer,
                                    item.asJsonObject(),
                                    subject,
                                    property);
                        }
                    }
                }
            }
        }
    }

    public ToRdf uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    /*
     * @see <a href=
     * "https://w3c.github.io/json-ld-api/#deserialize-json-ld-to-rdf-algorithm">
     * Object to RDF Conversion</a>
     */
    private void fromObject(
            final RdfTripleConsumer consumer,
            final JsonObject item,
            final String subject,
            final String predicate) throws JsonLdError, RdfConsumerException {

        // 1. - 2.
        if (NodeObject.isNodeObject(item)) {

            JsonValue id = item.get(Keywords.ID);

            if (JsonUtils.isNotString(id) || JsonUtils.isNull(id)) {
                return;
            }

            String idString = ((JsonString) id).getString();

            if (BlankNode.isWellFormed(idString) || UriUtils.isAbsoluteUri(idString, uriValidation)) {
                consumer.triple(subject, predicate, idString);
            }

            return;
        }

        // 3.
        if (ListNode.isListObject(item)) {
            fromList(consumer, item.get(Keywords.LIST).asJsonArray(), subject, predicate);
        }

        // 4.
        if (!ValueNode.isValueObject(item)) {
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
            valueString = Jcs.canonize(value, JakartaAdapter.instance());
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
                consumer.triple(
                        subject, 
                        predicate,
                        valueString, language, item.getString(Keywords.DIRECTION));

                // 13.3.
            } else if (RdfDirection.COMPOUND_LITERAL == rdfDirection) {

                final String blankNodeId = nodeMap.createIdentifier();

                // 13.3.2.
                consumer.triple(
                        blankNodeId,
                        RdfConstants.VALUE,
                        valueString,
                        XsdConstants.STRING);

                // 13.3.3.
                if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {
                    consumer.triple(
                            blankNodeId,
                            RdfConstants.LANGUAGE,
                            item.getString(Keywords.LANGUAGE).toLowerCase(),
                            XsdConstants.STRING);
                }

                // 13.3.4.
                consumer.triple(
                        blankNodeId,
                        RdfConstants.DIRECTION,
                        item.getString(Keywords.DIRECTION),
                        XsdConstants.STRING);

                consumer.triple(subject, predicate, blankNodeId);
                return;
            }

            // 14.
        } else {
            if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {
                consumer.triple(
                        subject,
                        predicate,
                        valueString, item.getString(Keywords.LANGUAGE), null);

            } else {
                consumer.triple(
                        subject,
                        predicate,
                        valueString, datatype);
            }
        }
    }

    /*
     * @see <a href="https://w3c.github.io/json-ld-api/#list-to-rdf-conversion">List
     * to RDF Conversion</a>
     */
    private void fromList(
            final RdfTripleConsumer consumer,
            final JsonArray list,
            final String subject,
            final String predicate) throws JsonLdError, RdfConsumerException {

        // 1.
        if (JsonUtils.isEmptyArray(list)) {
            consumer.triple(subject, predicate, RdfConstants.NIL);
            return;
        }

        // 2.
        final String[] bnodes = new String[list.size()];

        IntStream.range(0, bnodes.length).forEach(i -> bnodes[i] = nodeMap.createIdentifier());

        consumer.triple(subject, predicate, bnodes[0]);

        // 3.
        int index = 0;
        for (final JsonValue item : list) {

            final String blankNodeSubject = bnodes[index];
            index++;

            fromObject(
                    consumer,
                    item.asJsonObject(),
                    blankNodeSubject,
                    RdfConstants.FIRST);

            // 3.4.
            if (index < bnodes.length) {
                consumer.triple(blankNodeSubject, RdfConstants.REST, bnodes[index]);

            } else {
                consumer.triple(blankNodeSubject, RdfConstants.REST, RdfConstants.NIL);
            }
        }
    }

    private static final String toXsdDouble(BigDecimal bigDecimal) {
        return xsdNumberFormat.format(bigDecimal);
    }
}
