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
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Consumer;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.engine.Action;
import com.apicatalog.jsonld.engine.DocumentConsumer;
import com.apicatalog.jsonld.engine.Runtime;
import com.apicatalog.jsonld.engine.State;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public class ExpansionEngine implements Runtime {

    final Deque<Action> stack = new ArrayDeque<>();

    // mandatory
    private Context activeContext;
    private JsonValue element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    public ExpansionEngine(
            final Context activeContext,
            final JsonValue element,
            final String activeProperty,
            final URI baseUrl) {
        this.activeContext = activeContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
        this.fromMap = false;

        this.stack.push(this::expand);
    }

    public State execute() {

//        stack.peek().accept(this);

        return null;
    }

    public ExpansionEngine frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    public ExpansionEngine ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ExpansionEngine fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    @Override
    public void execute(Consumer<Runtime> fnc) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void fetch(String uri, DocumentConsumer consumer) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public <O> void complete(O output) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public <I> I input() {
        // TODO Auto-generated method stub
        return null;
    }
    
    protected void expand(Runtime runtime) throws JsonLdError, IOException {

        // 1. If element is null, return null
        if (JsonUtils.isNull(element)) {
            runtime.complete(null);
            return;
        }

        // 5. If element is an array,
        if (JsonUtils.isArray(element)) {

//            return ArrayExpansion
//                    .with(activeContext, element.asJsonArray(), activeProperty, baseUrl)
//                    .frameExpansion(frameExpansion)
//                    .ordered(ordered)
//                    .fromMap(fromMap)
//                    .expand();
        }

        // 3. If active property has a term definition in active context with a local
        // context, initialize property-scoped context to that local context.
        final JsonValue propertyContext = activeContext
                .getTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);

        // 4. If element is a scalar
        if (JsonUtils.isScalar(element)) {
            runtime.complete(ScalarExpansion.expand(
                    activeContext,
                    activeProperty,
                    propertyContext,
                    element));
            return;
        }

        // 6. Otherwise element is a map
//        return ObjectExpansion
//                .with(activeContext, propertyContext, element.asJsonObject(), activeProperty, baseUrl)
//                .frameExpansion(frameExpansion && !Keywords.DEFAULT.equals(activeProperty))
//                .ordered(ordered)
//                .fromMap(fromMap)
//                .expand();
    }
}