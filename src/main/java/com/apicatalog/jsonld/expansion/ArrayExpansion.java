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

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonArray;
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
    private Context activeContext;
    private JsonArray element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private ArrayExpansion(final Context activeContext, final JsonArray element, final String activeProperty,
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

    public static final ArrayExpansion with(final Context activeContext, final JsonArray element,
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

    public Collection<?> expand() throws JsonLdError {

//        final JsonArrayBuilder result = JsonProvider.instance().createArrayBuilder();

        final Set<Object> result = new LinkedHashSet<>(element.size());

        // 5.2.
        for (final JsonValue item : element) {

            activeContext.runtime().tick();

            // 5.2.1
            Object expanded = Expansion
                    .with(activeContext, item, activeProperty, baseUrl)
                    .frameExpansion(frameExpansion)
                    .ordered(ordered)
                    .fromMap(fromMap)
                    .compute();

            // 5.2.2
            if (expanded instanceof Collection<?> list
                    && activeContext.getTerm(activeProperty)
                            .map(TermDefinition::getContainerMapping)
                            .filter(c -> c.contains(Keywords.LIST)).isPresent()) {

//                expanded = ListNode.toListNode(expanded);
                expanded = List.of(list);
            }

            // 5.2.3
//            if (JsonUtils.isArray(expanded)) {
            if (expanded instanceof Set<?> set) {

                set.stream()
                        .filter(Objects::nonNull)
                        .forEach(result::add);

//                expanded.asJsonArray()

                // append non-null element
            } else if (expanded != null) {
                result.add(expanded);
            }

        }

        // 5.3
        return result;
    }
}
