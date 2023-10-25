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
package no.hasmac.jsonld.json;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import no.hasmac.jsonld.lang.Utils;
import no.hasmac.rdf.io.nquad.NQuadsWriter;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://tools.ietf.org/html/draft-rundgren-json-canonicalization-scheme-17">JSON Canonicalization Scheme (JCS)</a>
 *
 */
public final class JsonCanonicalizer {

    private static final DecimalFormat eFormatBigDecimal =
            new DecimalFormat("0E00", new DecimalFormatSymbols(Locale.ENGLISH));

    private static final DecimalFormat eFormat =
            new DecimalFormat("0.#######", new DecimalFormatSymbols(Locale.ENGLISH));

    private JsonCanonicalizer() {
    }

    public static String canonicalize(final JsonValue value) {

        final StringWriter writer = new StringWriter();

        try {

            canonicalize(value, writer);

        } catch (IOException e) {
            // ignore
        }

        return writer.toString();
    }

    private static void canonicalize(final JsonValue value, final Writer writer) throws IOException {

        if (JsonUtils.isNull(value)) {
            writer.write("null");

        } else if (JsonUtils.isScalar(value)) {

            if (JsonUtils.isNumber(value)) {

                canonicalizeNumber((JsonNumber)value, writer);

            } else {

                writer.write(value.toString());
            }

        } else if (JsonUtils.isArray(value)) {

            canonicalizeArray(value.asJsonArray(), writer);

        } else if (JsonUtils.isObject(value)) {

            canonicalizeObject(value.asJsonObject(), writer);

        }
    }

    private static void canonicalizeNumber(final JsonNumber number, final Writer writer) throws IOException {

        String numberString;

        if (number.bigDecimalValue().compareTo(BigDecimal.ZERO) == 0) {

            numberString = "0";

        } else if (number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

            numberString = eFormatBigDecimal.format(number.bigDecimalValue()).replace("E", "e+");

        } else if (number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointLeft(21)) <= 0) {

            numberString = eFormatBigDecimal.format(number.bigDecimalValue()).toLowerCase();

        } else {

            numberString = eFormat.format(number.bigDecimalValue());
        }

        writer.write(numberString);
    }

    private static void canonicalizeArray(final JsonArray value, final Writer writer) throws IOException {
        boolean next = false;

        writer.write("[");

        for (JsonValue item : value.asJsonArray()) {

            if (next) {
                writer.write(",");
            }

            canonicalize(item, writer);

            next = true;
        }

        writer.write("]");

    }

    private static void canonicalizeObject(final JsonObject value, final Writer writer) throws IOException {
        boolean next = false;

        writer.write("{");

        for (String propertyName : Utils.index(value.keySet(), true)) {

            if (next) {
                writer.write(",");
            }

            writer.write("\"");
            writer.write(NQuadsWriter.escape(propertyName));
            writer.write("\":");

            JsonValue propertyValue = value.get(propertyName);

            canonicalize(propertyValue, writer);

            next = true;
        }

        writer.write("}");
    }

}
