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

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonNumber;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value
 *      Expansion Algorithm</a>
 *
 */
public final class ValueExpansion {

    // required
    private final Context activeContext;

    // runtime
    private Optional<TermDefinition> definition;

    private ValueExpansion(final Context activeContext) {
        this.activeContext = activeContext;
    }

    public static final ValueExpansion with(final Context activeContext) {
        return new ValueExpansion(activeContext);
    }

    public Map<String, ?> expand(final JsonValue value, final String activeProperty) throws JsonLdError {
        System.out.println(">>>>>>>>>>>>>>>> " + value + ", " + activeProperty);
        definition = activeContext.getTerm(activeProperty);

        final Optional<String> typeMapping = definition.map(TermDefinition::getTypeMapping);

        if (typeMapping.isPresent()) {
            // 1.
            if (Keywords.ID.equals(typeMapping.get())) {

                String idValue = null;

                if (value instanceof JsonString jsonString) {
                    idValue = jsonString.getString();

                    // custom extension allowing to process numeric ids
                } else if (activeContext.runtime().isNumericId()
                        && value instanceof JsonNumber jsonNumber) {
                    idValue = jsonNumber.toString();
                }

                if (idValue != null) {
                    final String expandedValue = activeContext.uriExpansion().documentRelative(true)
                            .vocab(false).expand(idValue);

                    return Map.of(Keywords.ID, expandedValue);
                    // return JsonProvider.instance().createObjectBuilder().add(Keywords.ID,
                    // expandedValue).build();
                }

                // 2.
            } else if (Keywords.VOCAB.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = activeContext.uriExpansion().documentRelative(true)
                        .vocab(true).expand(((JsonString) value).getString());

                return Map.of(Keywords.ID, expandedValue);
//                return JsonProvider.instance().createObjectBuilder().add(Keywords.ID, expandedValue).build();
            }
        }

        // 3.
//        final JsonObjectBuilder result = JsonProvider.instance().createObjectBuilder().add(Keywords.VALUE, value);

        // 4.
        if (typeMapping
                .filter(t -> !Keywords.ID.equals(t) && !Keywords.VOCAB.equals(t) && !Keywords.NONE.equals(t))
                .isPresent()) {

//            result.add(Keywords.TYPE, typeMapping.get());
            return Map.of(Keywords.TYPE, typeMapping.get(),
                    Keywords.VALUE, JsonUtils.getScalar(value));
//            return new ValueNode(typeMapping.get(), value, null, null);

            // 5.
        }

        if (value instanceof JsonString jsonString) {

            // 5.1.
            final String language = definition
                    .map(TermDefinition::getLanguageMapping)
//                    .filter(JsonUtils::isString)
//?                    .filter(JsonUtils::isNotNull)
//                    .map(JsonString.class::cast)
//                    .map(JsonString::getString)
                    .orElseGet(() -> activeContext.getDefaultLanguage() != null
                            ? activeContext.getDefaultLanguage()
                            : null);

            // 5.2.
            final DirectionType direction = definition
                    .map(TermDefinition::getDirectionMapping)
                    .orElseGet(() -> activeContext.getDefaultBaseDirection());

            var map = new HashMap<String, Object>(3);
            map.put(Keywords.VALUE, jsonString.getString());

            // 5.3.
            if (language != null) {
                System.out.println("X XXXXXXXXXXXXX   " + language + " > " + activeContext.getDefaultLanguage());
                System.out.println(definition
                    .map(TermDefinition::getLanguageMapping).orElse(null));
                map.put(Keywords.LANGUAGE, language);
            }

            // 5.4.
            if (direction != null && !DirectionType.NULL.equals(direction)) {
                map.put(Keywords.DIRECTION, direction.name().toLowerCase());
            }
            return map;
        }

        // 6.
        return Map.of(Keywords.VALUE, JsonUtils.getScalar(value));
    }

}
