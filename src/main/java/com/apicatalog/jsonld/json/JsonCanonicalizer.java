package com.apicatalog.jsonld.json;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.stream.Collectors;

import javax.json.JsonNumber;
import javax.json.JsonValue;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/draft-rundgren-json-canonicalization-scheme-17">JSON Canonicalization Scheme (JCS)</a>
 *
 */
public final class JsonCanonicalizer {

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
            
            //TODO conversions
            
            if (JsonUtils.isNumber(value)) {
                writer.write(((JsonNumber)value).toString().toLowerCase());
                
            } else {
            
                writer.write(value.toString());
            }
            
        } else if (JsonUtils.isArray(value)) {
         
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
            
        } else if (JsonUtils.isObject(value)) {
            
            boolean next = false;
            
            writer.write("{");
            
            for (String propertyName : value.asJsonObject().keySet().stream().sorted().collect(Collectors.toList())) {
                
                if (next) {
                    writer.write(",");
                }
                
                writer.write("\"");
                writer.write(propertyName);
                writer.write("\":");
                
                JsonValue propertyValue = value.asJsonObject().get(propertyName);
                
                canonicalize(propertyValue, writer);
                
                next = true;
            }
            
            writer.write("}");
            
        }
    }
    
}
