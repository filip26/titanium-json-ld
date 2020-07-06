package com.apicatalog.jsonld.json;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.stream.Collectors;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.rdf.io.nquad.NQuadsWriter;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/draft-rundgren-json-canonicalization-scheme-17">JSON Canonicalization Scheme (JCS)</a>
 *
 */
public final class JsonCanonicalizer {

    private JsonCanonicalizer() {
    }
    
    public static final String canonicalize(final JsonValue value) {
        
        final StringWriter writer = new StringWriter();
        
        try {
        
            canonicalize(value, writer);
            
        } catch (IOException e) {
            // ignore
        }

        return writer.toString();
    }

    private static final void canonicalize(final JsonValue value, final Writer writer) throws IOException {
        
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

    private static final void canonicalizeNumber(final JsonNumber number, final Writer writer) throws IOException {
        
        String numberString;
        
        if (number.bigDecimalValue().compareTo(BigDecimal.ZERO) == 0) {
            
            numberString = "0";
        
        } else if (number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {
            
            numberString = (new DecimalFormat("0E00")).format(number.bigDecimalValue()).replace("E", "e+");
            
        } else if (number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointLeft(21)) <= 0) {
            
            numberString = (new DecimalFormat("0E00")).format(number.bigDecimalValue()).toLowerCase();
            
        } else {
            
            numberString = (new DecimalFormat("0.#######")).format(number.bigDecimalValue());
        }
        
        writer.write(numberString);        
    }
    
    private static final void canonicalizeArray(final JsonArray value, final Writer writer) throws IOException {
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
    
    private static final void canonicalizeObject(final JsonObject value, final Writer writer) throws IOException {
        boolean next = false;
        
        writer.write("{");
        
        for (String propertyName : value.keySet().stream().sorted().collect(Collectors.toList())) {
            
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
