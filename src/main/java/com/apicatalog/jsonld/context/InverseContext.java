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
package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public final class InverseContext {

    private final Map<String, Map<String, Map<String, Map<String, String>>>> context;

    public InverseContext() {
        this.context = new LinkedHashMap<>();
    }

    private void set(final String variable, final String container, final String type, final String key, final String value) {
        context.computeIfAbsent(variable, x -> new LinkedHashMap<>())
                .computeIfAbsent(container, x -> new LinkedHashMap<>())
                .computeIfAbsent(type, x -> new LinkedHashMap<>())
                .put(key, value);
    }

    private boolean doesNotContain(final String variable, final String container, final String type, final String key) {
        return !context.containsKey(variable)
                || !context.get(variable).containsKey(container)
                || !context.get(variable).get(container).containsKey(type)
                || !context.get(variable).get(container).get(type).containsKey(key);
    }

    public boolean contains(final String variable) {
        return context.containsKey(variable);
    }

    public boolean contains(final String variable, final String container, final String type) {
        return context.containsKey(variable) 
                && context.get(variable).containsKey(container)
                && context.get(variable).get(container).containsKey(type);
    }

    public boolean contains(final String variable, final String container, final String type, final String key) {
        return contains(variable)
                    && context.get(variable).containsKey(container)
                    && context.get(variable).get(container).containsKey(type)
                    && context.get(variable).get(container).get(type).containsKey(key);
    }

    public InverseContext setIfAbsent(final String variable, final String container, final String type, final String key, final String value) {
        if (doesNotContain(variable, container, type, key)) {
            set(variable, container, type, key, value);
        }
        return this;
    }

    public Optional<String> get(final String variable, final String container, final String type, final String key) {
        if (doesNotContain(variable, container, type, key)) {
            return Optional.empty();
        }
        return Optional.ofNullable(context.get(variable).get(container).get(type).get(key));
    }
}