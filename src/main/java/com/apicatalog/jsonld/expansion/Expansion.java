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

import java.io.IOException;
import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public final class Expansion {

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private Expansion() {
        // default values
        this.frameExpansion = false;
        this.ordered = false;
        this.fromMap = false;
    }

    public static final Expansion with() {
        return new Expansion();
    }

    public Object expand(
            Context activeContext,
            Object node,
            NodeAdapter nodeAdapter,
            String activeProperty,
            URI baseUrl

    ) throws JsonLdError, IOException {

        // 1. If element is null, return null
        if (nodeAdapter.isNull(node)) {
            return null;
        }
        
        var nodeType = nodeAdapter.type(node) ;

        // 5. If element is an array,
        if (nodeType == NodeType.COLLECTION) {
            return ArrayExpansion
                    .with()
                    .frameExpansion(frameExpansion)
                    .ordered(ordered)
                    .fromMap(fromMap)
                    .expand(activeContext, node, nodeAdapter, activeProperty, baseUrl);
        }

        // 3. If active property has a term definition in active context with a local
        // context, initialize property-scoped context to that local context.
        var propertyContext = activeContext
                .getTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);

        // 4. If element is a scalar
        if (nodeType.isScalar()) {
            return ScalarExpansion
                    .expand(activeContext, activeProperty, propertyContext, node, nodeAdapter);
        }

        // 6. Otherwise element is a map
        return ObjectExpansion
                .with(activeContext,
                        propertyContext,
                        node,
                        nodeAdapter,
                        activeProperty,
                        baseUrl)
                .frameExpansion(frameExpansion && !Keywords.DEFAULT.equals(activeProperty))
                .ordered(ordered)
                .fromMap(fromMap)
                .expand();
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
}