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

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;

final class RdfToObject {

    // required
    private RdfValue value;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    
    // optional
    private Version processingMode;
    
    private RdfToObject(final RdfValue object, final RdfDirection rdfDirection, final boolean useNativeTypes) {
        this.value = object;
        this.rdfDirection = rdfDirection;
        this.useNativeTypes = useNativeTypes;
        
        this.processingMode = null;
    }
    
    public static final RdfToObject with(final RdfValue object, final RdfDirection rdfDirection, final boolean useNativeTypes) {
        return new RdfToObject(object, rdfDirection, useNativeTypes);
    }
    
    public RdfToObject processingMode(Version processingMode) {
        this.processingMode = processingMode;
        return this;
    }
    
    public JsonObject build() throws JsonLdError {

        // 1.
        if (value.isIRI() || value.isBlankNode()) {
            return new JsonObjectRef(Json.createObjectBuilder().add(Keywords.ID, value.getValue()).build());
        }

        final JsonObjectBuilder result = Json.createObjectBuilder();

        // 2.
        final RdfLiteral literal = value.asLiteral();
        
        // 2.2.
        JsonValue convertedValue = Json.createValue(literal.getValue());
        
        // 2.3.
        String type = null;

        // 2.4.
        if (useNativeTypes) {
            
            if (literal.getDatatype() != null) {
            
                // 2.4.1.
                if (XsdConstants.STRING.equals(literal.getDatatype())) {
                    convertedValue = Json.createValue(literal.getValue());
    
                // 2.4.2.
                } else if (XsdConstants.BOOLEAN.equals(literal.getDatatype())) {
                    
                    if ("true".equalsIgnoreCase(literal.getValue())) {
                    
                        convertedValue = JsonValue.TRUE;
                        
                    } else if ("false".equalsIgnoreCase(literal.getValue())) {
    
                        convertedValue = JsonValue.FALSE;
                        
                    } else {
    
                        type = XsdConstants.BOOLEAN;
                    }
                    
                // 2.4.3.                
                } else if (XsdConstants.INTEGER.equals(literal.getDatatype())) {
                    
                    convertedValue = Json.createValue(Long.parseLong(literal.getValue()));
                    
                } else if (XsdConstants.DOUBLE.equals(literal.getDatatype())) {
                    
                    convertedValue = Json.createValue(Double.parseDouble(literal.getValue()));
                    
                } else if (literal.getDatatype() != null) {
                    
                    type = literal.getDatatype();
                }
            }

        // 2.5.
        } else if (processingMode != Version.V1_0 
                        && literal.getDatatype() != null 
                        && RdfConstants.JSON.equals(literal.getDatatype())) {

            try (JsonParser parser = Json.createParser(new StringReader(literal.getValue()))) {
                
                parser.next();
                
                convertedValue = parser.getValue();
                type = Keywords.JSON;
                
            } catch (Exception e) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_JSON_LITERAL, e);
            }
            
        // 2.6.
        } else if (RdfDirection.I18N_DATATYPE == rdfDirection
                    && literal.getDatatype() != null 
                    && literal.getDatatype().startsWith(RdfConstants.I18N_BASE)
                ) {

            convertedValue = Json.createValue(literal.getValue());

            String langId = literal.getDatatype().substring(RdfConstants.I18N_BASE.length());
            
            int directionIndex = langId.indexOf('_');

            if (directionIndex > 1) {
                
                result.add(Keywords.LANGUAGE, Json.createValue(langId.substring(0, directionIndex)));
                result.add(Keywords.DIRECTION, Json.createValue(langId.substring(directionIndex + 1)));
                
            } else if (directionIndex == 0) {
                
                result.add(Keywords.DIRECTION, Json.createValue(langId.substring(1)));
                
            } else  if (directionIndex == -1) {
                
                result.add(Keywords.LANGUAGE, Json.createValue(langId));
            }
            
        // 2.7. 
        } else if (literal.getLanguage().isPresent()) {
            
            result.add(Keywords.LANGUAGE, Json.createValue(literal.getLanguage().get()));

        // 2.8.   
        } else if (literal.getDatatype() != null 
                        && !XsdConstants.STRING.equals(literal.getDatatype())) {
            
            type = literal.getDatatype();
        }        

        // 2.9.
        result.add(Keywords.VALUE, convertedValue);
   
        // 2.10.
        if (type != null) {
            result.add(Keywords.TYPE, Json.createValue(type));
        }

        // 2.11.
        return result.build();
    }
}
