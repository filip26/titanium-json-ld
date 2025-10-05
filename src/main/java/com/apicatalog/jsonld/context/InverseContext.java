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

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public final class InverseContext {

    private final Set<String> variables;
    private final Map<Integer, Map<String, String>> mapping;

    public InverseContext() {
        this.variables = new HashSet<>();
        this.mapping = new LinkedHashMap<>();
    }

    private void set(final String variable, final String container, final String type, final String key, final String value) {
        variables.add(variable);
        mapping.computeIfAbsent(
                Objects.hash(variable, container, type),
                c -> new LinkedHashMap<>()).put(key, value);
    }

    public boolean contains(final String variable) {
        return variables.contains(variable);
    }

    public boolean contains(final String variable, final String container, final String type) {
        return mapping.containsKey(Objects.hash(variable, container, type));
    }

    private boolean doesNotContain(final String variable, final String container, final String type, final String key) {
        return Optional.ofNullable(mapping.get(Objects.hash(variable, container, type)))
                .filter(m -> m.containsKey(key))
                .isEmpty();
    }

    public InverseContext setIfAbsent(final String variable, final String container, final String type, final String key, final String value) {
        if (doesNotContain(variable, container, type, key)) {
            set(variable, container, type, key, value);
        }
        return this;
    }

    public Optional<String> get(final String variable, final String container, final String type, final String key) {
        return Optional.ofNullable(mapping.get(Objects.hash(variable, container, type)))
                .map(c -> c.get(key));
    }
}