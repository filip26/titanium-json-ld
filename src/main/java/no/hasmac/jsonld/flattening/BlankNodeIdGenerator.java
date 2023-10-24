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
package no.hasmac.jsonld.flattening;

import java.util.HashMap;
import java.util.Map;

import no.hasmac.jsonld.StringUtils;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-api/#generate-blank-node-identifier">Generate Blank Node Identifier</a>
 */
public final class BlankNodeIdGenerator {

    private final Map<String, String> map;

    private int counter;

    public BlankNodeIdGenerator() {
        this.map = new HashMap<>();
        this.counter = 0;
    }

    public String createIdentifier() {
     return "_:b" + counter++;
    }

    public String createIdentifier(String identifier) {
        if (identifier == null || StringUtils.isBlank(identifier)) {
            return createIdentifier();
        }

        return map.computeIfAbsent(identifier, x -> createIdentifier());

    }

}
