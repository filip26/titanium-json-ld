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
package com.apicatalog.jsonld.flattening;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A generator for blank node identifiers.
 * <p>
 * This class creates unique blank node identifiers and ensures that the same
 * input identifier consistently maps to the same generated blank node
 * identifier within the scope of a single instance. This is crucial for
 * algorithms like JSON-LD flattening.
 * </p>
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-api/#generate-blank-node-identifier">Generate
 *      Blank Node Identifier</a>
 */
public final class BlankNodeIdGenerator {

    /**
     * The default prefix for generated blank node identifiers.
     */
    public static final String PREFIX = "_:b";

    private final Map<String, String> map;
    private final String prefix;
    private int counter;

    /**
     * Creates a new {@code BlankNodeIdGenerator} with default settings. It
     * initializes with an empty identifier map, the default prefix "{@code _:b}",
     * and a counter starting at 0.
     */
    public BlankNodeIdGenerator() {
        this(new HashMap<>(), PREFIX, 0);
    }

    /**
     * Creates a new {@code BlankNodeIdGenerator} with a custom initial state.
     *
     * @param map     the initial map of existing identifiers to generated
     *                identifiers
     * @param prefix  the prefix to use for new identifiers
     * @param counter the starting value for the identifier counter
     */
    public BlankNodeIdGenerator(Map<String, String> map, String prefix, int counter) {
        this.map = new HashMap<>(Objects.requireNonNull(map, "Identifier map cannot be null."));
        this.prefix = Objects.requireNonNull(prefix, "Prefix cannot be null.");
        this.counter = counter;
    }

    /**
     * Generates a new, unique blank node identifier unconditionally.
     * <p>
     * The identifier is created by concatenating the prefix with the current
     * counter value, and the counter is then incremented for the next call.
     * </p>
     *
     * @return a new blank node identifier (e.g., "{@code _:b0}", "{@code _:b1}",
     *         etc.)
     */
    public String createIdentifier() {
        return prefix + (counter++);
    }

    /**
     * Retrieves or generates a blank node identifier for a given input identifier.
     * <p>
     * If the provided {@code identifier} is {@code null} or blank, this method
     * generates a new, unique identifier.
     * </p>
     * <p>
     * If the {@code identifier} has been seen before, the previously generated
     * identifier is returned to ensure consistency.
     * </p>
     * <p>
     * If the {@code identifier} is new, a new unique identifier is generated,
     * stored for future lookups, and then returned.
     * </p>
     *
     * @param identifier the existing identifier to map to a blank node identifier
     *                   (may be {@code null} or blank)
     * @return the corresponding blank node identifier, either newly generated or
     *         retrieved from the cache
     */
    public String createIdentifier(final String identifier) {
        if (identifier == null || identifier.isBlank()) {
            return createIdentifier();
        }

        return map.computeIfAbsent(identifier, k -> createIdentifier());
    }
}
