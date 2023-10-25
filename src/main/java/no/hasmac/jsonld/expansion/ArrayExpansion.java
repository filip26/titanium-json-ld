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
import java.util.Optional;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.lang.ListObject;

import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

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

    public static ArrayExpansion with(final ActiveContext activeContext, final JsonArray element,
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

    public JsonArray expand() throws JsonLdError {

        // 5.1
        final JsonArrayBuilder result = JsonProvider.instance().createArrayBuilder();

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
            if (definition
                        .map(TermDefinition::getContainerMapping)
                        .filter(c -> c.contains(Keywords.LIST)).isPresent()
                    && JsonUtils.isArray(expanded)) {

                expanded = ListObject.toListObject(expanded);
            }

            // 5.2.3
            if (JsonUtils.isArray(expanded)) {

                // append array
                expanded
                    .asJsonArray()
                    .stream()
                    .filter(JsonUtils::isNotNull)
                    .forEach(result::add);

            // append non-null element
            } else if (JsonUtils.isNotNull(expanded)) {
                result.add(expanded);
            }
        }

        // 5.3
        return result.build();
    }
}
