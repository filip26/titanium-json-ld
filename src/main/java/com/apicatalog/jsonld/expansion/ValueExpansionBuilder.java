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
public final class ValueExpansionBuilder {

    // required
    private final ActiveContext activeContext;
    private final String activeProperty;
    private final JsonValue value;
    
    // runtime
    private JsonObject result;
    private Optional<TermDefinition> definition;

    public ValueExpansionBuilder(final ActiveContext activeContext, final JsonValue value,
            final String activeProperty) {
        
        this.activeContext = activeContext;
        this.value = value;
        this.activeProperty = activeProperty;
    }

    public static final ValueExpansionBuilder with(final ActiveContext activeContext, final JsonValue element,
            final String activeProperty) {
        return new ValueExpansionBuilder(activeContext, element, activeProperty);
    }

    public JsonValue build() throws JsonLdError {

        definition = activeContext.getTerm(activeProperty);

        final Optional<String> typeMapping = definition.map(TermDefinition::getTypeMapping);

        if (typeMapping.isPresent()) {

            // 1.
            if (Keywords.ID.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = activeContext.uriExpansion(((JsonString) value).getString()).documentRelative(true)
                        .vocab(false).build();

                return Json.createObjectBuilder().add(Keywords.ID, expandedValue).build();
            }

            // 2.
            if (Keywords.VOCAB.equals(typeMapping.get()) && JsonUtils.isString(value)) {

                String expandedValue = activeContext.uriExpansion(((JsonString) value).getString()).documentRelative(true)
                        .vocab(true).build();

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
    
    private void buildStringValue() throws JsonLdError {

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
