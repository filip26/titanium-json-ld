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

import java.net.URI;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class Expansion {

    // mandatory
    private ActiveContext activeContext;
    private JsonValue element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private Expansion(final ActiveContext activeContext, final JsonValue element, final String activeProperty,
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

    public static Expansion with(final ActiveContext activeContext, final JsonValue element, final String activeProperty, final URI baseUrl) {
        return new Expansion(activeContext, element, activeProperty, baseUrl);
    }

    public Expansion frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    public Expansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public Expansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    public JsonValue compute() throws JsonLdError {

        // 1. If element is null, return null
        if (JsonUtils.isNull(element)) {
            return JsonValue.NULL;
        }

        // 5. If element is an array,
        if (JsonUtils.isArray(element)) {

            return ArrayExpansion
                        .with(activeContext, element.asJsonArray(), activeProperty, baseUrl)
                        .frameExpansion(frameExpansion)
                        .ordered(ordered)
                        .fromMap(fromMap)
                        .expand();
        }

        // 3. If active property has a term definition in active context with a local
        // context, initialize property-scoped context to that local context.
        final JsonValue propertyContext = activeContext
                                            .getTerm(activeProperty)
                                            .map(TermDefinition::getLocalContext)
                                            .orElse(null);

        // 4. If element is a scalar
        if (JsonUtils.isScalar(element)) {

            return ScalarExpansion
                        .with(activeContext, propertyContext, element, activeProperty)
                        .expand();
        }

        // 6. Otherwise element is a map
        return ObjectExpansion
                    .with(activeContext, propertyContext, element.asJsonObject(), activeProperty, baseUrl)
                    .frameExpansion(frameExpansion && !Keywords.DEFAULT.equals(activeProperty))
                    .ordered(ordered)
                    .fromMap(fromMap)
                    .expand();
    }
}
