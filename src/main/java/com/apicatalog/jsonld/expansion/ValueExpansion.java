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
package com.apicatalog.jsonld.expansion;

import java.util.Optional;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
 *      Expansion Algorithm</a>
 *
 */
public final class ValueExpansion {

    // required
    private final ActiveContext activeContext;
    
    // runtime
    private JsonObject result;
    private Optional<TermDefinition> definition;

    private ValueExpansion(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }

    public static final ValueExpansion with(final ActiveContext activeContext) {
        return new ValueExpansion(activeContext);
    }

    public JsonObject expand(final JsonValue value, final String activeProperty) throws JsonLdError {

        definition = activeContext.getTerm(activeProperty);

        final Optional<String> typeMapping = definition.map(TermDefinition::getTypeMapping);

        if (typeMapping.isPresent()) {

            // 1.
            if (Keywords.ID.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = activeContext.uriExpansion().documentRelative(true)
                        .vocab(false).expand(((JsonString) value).getString());

                return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
            }

            // 2.
            if (Keywords.VOCAB.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = activeContext.uriExpansion().documentRelative(true)
                        .vocab(true).expand(((JsonString) value).getString());

                return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
            }
        }

        // 3.
        result = Json.createObjectBuilder().add(Keywords.VALUE, value).build();

        // 4.
        if (typeMapping.isPresent() && !Keywords.ID.equals(typeMapping.get()) && !Keywords.VOCAB.equals(typeMapping.get())
                && !Keywords.NONE.equals(typeMapping.get())) {

            result = Json.createObjectBuilder(result).add(Keywords.TYPE, Json.createValue(typeMapping.get())).build();

            // 5.
        } else if (JsonUtils.isString(value)) {
            buildStringValue();
        }

        // 6.
        return result;
    }
    
    private void buildStringValue() {

        // 5.1.
        JsonValue language = null;

        if (definition.isPresent() && definition.map(TermDefinition::getLanguageMapping).isPresent()) {
            language = definition.get().getLanguageMapping();

        } else if (activeContext.getDefaultLanguage() != null) {
            language = Json.createValue(activeContext.getDefaultLanguage());
        }

        // 5.2.
        final DirectionType direction = definition
                                            .map(TermDefinition::getDirectionMapping)
                                            .orElse(activeContext.getDefaultBaseDirection());
        
        // 5.3.
        if (JsonUtils.isNotNull(language)) {
            result = Json.createObjectBuilder(result).add(Keywords.LANGUAGE, language).build();
        }

        // 5.4.
        if (direction != null && !DirectionType.NULL.equals(direction)) {
            result = Json.createObjectBuilder(result)
                    .add(Keywords.DIRECTION, Json.createValue(direction.name().toLowerCase())).build();
        }
    }
}
