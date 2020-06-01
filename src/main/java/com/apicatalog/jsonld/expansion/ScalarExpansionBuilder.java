package com.apicatalog.jsonld.expansion;

import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;

/**
 * Steps 4.1 - 4.3
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class ScalarExpansionBuilder {

    // mandatory
    private ActiveContext activeContext;
    private JsonValue propertyContext;
    private JsonValue element;
    private String activeProperty;

    private ScalarExpansionBuilder(final ActiveContext activeContext, final JsonValue propertyContext,
            final JsonValue element, final String activeProperty) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.activeProperty = activeProperty;
    }

    public static final ScalarExpansionBuilder with(final ActiveContext activeContext, final JsonValue propertyContext,
            final JsonValue element, final String activeProperty) {
        return new ScalarExpansionBuilder(activeContext, propertyContext, element, activeProperty);
    }

    public JsonValue compute() throws JsonLdError {

        /*
         * 4.1. If active property is null or @graph, drop the free-floating scalar by
         * returning null.
         */
        if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {
            return JsonValue.NULL;
        }

        /*
         * 4.2. If property-scoped context is defined, set active context to the result
         * of the Context Processing algorithm, passing active context, property-scoped
         * context as local context, and base URL from the term definition for active
         * property in active context.
         */
        if (propertyContext != null) {

            final TermDefinition definition = activeContext.getTerm(activeProperty);

            activeContext = activeContext
                                .create(propertyContext, definition.getBaseUrl())
                                .build();
        }

        /*
         * 4.3. Return the result of the Value Expansion algorithm, passing the active
         * context, active property, and element as value.
         */
        return ValueExpansionBuilder.with(activeContext, element, activeProperty).build();
    }
}
