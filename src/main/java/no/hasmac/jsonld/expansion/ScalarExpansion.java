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
package no.hasmac.jsonld.expansion;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.JsonValue;

/**
 * Steps 4.1 - 4.3
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class ScalarExpansion {

    // mandatory
    private ActiveContext activeContext;
    private JsonValue propertyContext;
    private JsonValue element;
    private String activeProperty;

    private ScalarExpansion(final ActiveContext activeContext, final JsonValue propertyContext,
            final JsonValue element, final String activeProperty) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.activeProperty = activeProperty;
    }

    public static ScalarExpansion with(final ActiveContext activeContext, final JsonValue propertyContext,
                                       final JsonValue element, final String activeProperty) {
        return new ScalarExpansion(activeContext, propertyContext, element, activeProperty);
    }

    public JsonValue expand() throws JsonLdError {

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
            activeContext = activeContext
                                .newContext()
                                .create(
                                    propertyContext,
                                    activeContext.getTerm(activeProperty).map(TermDefinition::getBaseUrl).orElse(null)
                                );
        }

        /*
         * 4.3. Return the result of the Value Expansion algorithm, passing the active
         * context, active property, and element as value.
         */
        return activeContext.valueExpansion().expand(element, activeProperty);
    }
}
