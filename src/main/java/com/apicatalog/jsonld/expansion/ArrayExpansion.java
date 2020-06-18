package com.apicatalog.jsonld.expansion;

import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;

/**
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class ArrayExpansion {

    // mandatory
    private ActiveContext activeContext;
    private JsonArray element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private ArrayExpansion(final ActiveContext activeContext, final JsonArray element, final String activeProperty,
            final URI baseUrl) {
        this.activeContext = activeContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
        this.fromMap = false;
    }

    public static final ArrayExpansion with(final ActiveContext activeContext, final JsonArray element,
            final String activeProperty, final URI baseUrl) {
        return new ArrayExpansion(activeContext, element, activeProperty, baseUrl);
    }

    public ArrayExpansion frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    public ArrayExpansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ArrayExpansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    public JsonValue compute() throws JsonLdError {

        // 5.1
        final JsonArrayBuilder result = Json.createArrayBuilder();

        // 5.2.
        for (final JsonValue item : element) {

            // 5.2.1
            JsonValue expanded = 
                            Expansion
                                .with(activeContext, item, activeProperty, baseUrl)
                                .frameExpansion(frameExpansion)
                                .ordered(ordered)
                                .fromMap(fromMap)
                                .compute();

            final Optional<TermDefinition> definition = activeContext.getTerm(activeProperty);

            // 5.2.2
            if (definition.isPresent() && definition.get().getContainerMapping() != null
                    && definition.get().getContainerMapping().contains(Keywords.LIST) && JsonUtils.isArray(expanded)) {

                expanded = ListObject.toListObject(expanded);
            }

            // 5.2.3
            if (JsonUtils.isArray(expanded)) {

                // append array
                for (JsonValue expandedItem : expanded.asJsonArray()) {

                    if (JsonUtils.isNull(expandedItem)) {
                        continue;
                    }

                    result.add(expandedItem);
                }

                // append non-null element
            } else if (JsonUtils.isNotNull(expanded)) {
                result.add(expanded);
            }
        }

        // 5.3
        return result.build();
    }
}
