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
import java.util.function.Function;
import java.util.stream.Collectors;

public final class InverseContext {

    final Map<InverseDefinition, String> definitions;

    public InverseContext() {
        this.definitions = new LinkedHashMap<>(16);
    }

    void set(final String variable, final String container, final String type, final String key, final String value) {
        definitions.put(new InverseDefinition(variable, container, type, key), value);
    }

    public boolean contains(final String variable) {
        return definitions.keySet().stream()
                .map(InverseDefinition::variable)
                .anyMatch(variable::equals);
    }

    public Map<String, InverseDefinition> definitions(final String variable, final String container, final String type) {
        return definitions.keySet().stream()
                .filter(def -> def.match(variable, container, type))
                .collect(Collectors.toUnmodifiableMap(
                        InverseDefinition::key,
                        Function.identity()));
    }

    boolean contains(final String variable, final String container, final String type, final String key) {
        return definitions.keySet().stream()
                .anyMatch(def -> def.match(variable, container, type, key));
    }

    public InverseContext setIfAbsent(final String variable, final String container, final String type, final String key, final String value) {
        if (contains(variable, container, type, key)) {
            return this;
        }
        set(variable, container, type, key, value);
        return this;
    }

    public String value(final InverseDefinition def) {
        return definitions.get(def);
    }
}