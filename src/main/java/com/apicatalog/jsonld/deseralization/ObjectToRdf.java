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
import java.util.List;
import java.util.Locale;
import java.util.Optional;

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
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;

import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-api/#deserialize-json-ld-to-rdf-algorithm">Object to RDF Conversion</a>
 *
 */
final class ObjectToRdf {

    private static final DecimalFormat xsdNumberFormat =
            new DecimalFormat("0.0##############E0", new DecimalFormatSymbols(Locale.ENGLISH));

    static { xsdNumberFormat.setMinimumFractionDigits(1); }

    // required
    private JsonObject item;
    private List<RdfTriple> triples;
    private NodeMap nodeMap;

    // optional
    private RdfDirection rdfDirection;
    private boolean uriValidation;

    private ObjectToRdf(JsonObject item, List<RdfTriple> triples, NodeMap nodeMap) {
        this.item = item;
        this.triples = triples;
        this.nodeMap = nodeMap;

        // default values
        this.rdfDirection = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static final ObjectToRdf with(JsonObject item, List<RdfTriple> triples, NodeMap nodeMap) {
        return  new ObjectToRdf(item, triples, nodeMap);
    }

    public ObjectToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public Optional<RdfValue> build() throws JsonLdError {

        // 1. - 2.
        if (NodeObject.isNodeObject(item)) {

            JsonValue id = item.get(Keywords.ID);

            if (JsonUtils.isNotString(id) || JsonUtils.isNull(id)) {
                return Optional.empty();
            }

            String idString = ((JsonString)id).getString();

            if (BlankNode.isWellFormed(idString)) {
                return Optional.of(Rdf.createBlankNode(idString));

            } else if (UriUtils.isAbsoluteUri(idString, uriValidation)) {
                return Optional.of(Rdf.createIRI(idString));
            }

            return Optional.empty();
        }

        // 3.
        if (ListObject.isListObject(item)) {
            return Optional.of(ListToRdf
                        .with(item.get(Keywords.LIST).asJsonArray(), triples, nodeMap)
                        .rdfDirection(rdfDirection)
                        .uriValidation(uriValidation)
                        .build());
        }

        // 4.
        if (!ValueObject.isValueObject(item)) {
            return Optional.empty();
        }

        final JsonValue value = item.get(Keywords.VALUE);

        // 5.
        String datatype = item.containsKey(Keywords.TYPE) && JsonUtils.isString(item.get(Keywords.TYPE))
                            ? item.getString(Keywords.TYPE)
                            : null;

        // 6.
        if (datatype != null && !Keywords.JSON.equals(datatype) && !UriUtils.isAbsoluteUri(datatype, uriValidation)) {
            return Optional.empty();
        }

        // 7.
        if (item.containsKey(Keywords.LANGUAGE) && (JsonUtils.isNotString(item.get(Keywords.LANGUAGE))
                || !LanguageTag.isWellFormed(item.getString(Keywords.LANGUAGE)))
                ) {

            return Optional.empty();
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

            JsonNumber number = ((JsonNumber)value);


            // 11.
            if ((!number.isIntegral()  && number.doubleValue() % -1 != 0)
                    || XsdConstants.DOUBLE.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0
                    ) {

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
                                : XsdConstants.STRING
                                ;
        }

        if (valueString == null) {

            if (JsonUtils.isNotString(value)) {
                return Optional.empty();
            }

            valueString = ((JsonString)value).getString();
        }

        RdfLiteral rdfLiteral = null;

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

                rdfLiteral = Rdf.createTypedString(valueString, datatype);

            // 13.3.
            } else if (RdfDirection.COMPOUND_LITERAL == rdfDirection) {

                final String blankNodeId = nodeMap.createIdentifier();

                // 13.3.1.
                final RdfResource subject = Rdf.createBlankNode(blankNodeId);

                // 13.3.2.
                triples.add(Rdf.createTriple(
                                    subject,
                                    Rdf.createIRI(RdfConstants.VALUE),
                                    Rdf.createString(valueString))
                                    );

                // 13.3.3.
                if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {
                    triples.add(Rdf.createTriple(
                                    subject,
                                    Rdf.createIRI(RdfConstants.LANGUAGE),
                                    Rdf.createString(item.getString(Keywords.LANGUAGE).toLowerCase()))
                                    );
                }

                // 13.3.4.
                triples.add(Rdf.createTriple(
                                    subject,
                                    Rdf.createIRI(RdfConstants.DIRECTION),
                                    Rdf.createString(item.getString(Keywords.DIRECTION)))
                                    );

                return Optional.of(Rdf.createBlankNode(blankNodeId));
            }

        // 14.
        } else {
            if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {

                rdfLiteral = Rdf.createLangString(valueString, item.getString(Keywords.LANGUAGE));

            } else {
                rdfLiteral = Rdf.createTypedString(valueString, datatype);
            }
        }

        // 15.
        return Optional.ofNullable(rdfLiteral);
    }

    private static final String toXsdDouble(BigDecimal bigDecimal) {
        return xsdNumberFormat.format(bigDecimal);
    }

    public ObjectToRdf uriValidation(boolean uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }
}
