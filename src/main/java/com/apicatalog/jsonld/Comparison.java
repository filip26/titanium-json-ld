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
package com.apicatalog.jsonld;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.jsonld.lang.Keywords;

/**
 * Structural equality for JSON-LD values as defined by the
 * <a href="https://www.w3.org/TR/json-ld11-api/#comparison">JSON-LD 1.1 Object
 * Comparison</a> algorithm.
 * <p>
 * The comparison does not rely on Java {@link Object#equals(Object)}. Instead,
 * it determines whether two nodes represent the same logical JSON-LD value
 * regardless of serialization differences or adapter implementations.
 * </p>
 *
 * <h2>Comparison semantics</h2>
 * <ul>
 * <li><b>Scalars</b> ({@code null}, {@code true}, {@code false}, strings,
 * numbers, binary values) are compared by value.</li>
 * <li><b>Maps</b> are compared recursively by key and value. Key order is
 * ignored.</li>
 * <li><b>Arrays</b> are compared as unordered collections, except when the
 * array is the value of an {@code @list} property, in which case order
 * matters.</li>
 * </ul>
 *
 * <p>
 * This class is stateless and thread-safe. All methods are static.
 * </p>
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#comparison"> JSON-LD 1.1
 *      Object Comparison</a>
 */
public final class Comparison {

    private Comparison() {
    }

    /**
     * Compares two node.
     *
     * @return {@code true} if both nodes are JSON-LD equivalent
     */
    public static final boolean equals(
            final Object value1,
            final Object value2) {

        return equals(value1, value2, null);
    }

    private static final boolean equals(
            final Object value1,
            final Object value2,
            final Object parentProperty) {

        if (value1 == value2) {
            return true;
        }

        if (value1 == null || value2 == null) {
            return false;
        }

        if (value1 instanceof Map map1 && value2 instanceof Map map2) {
            return objectEquals(map1, map2);

        }

        if (value1 instanceof Collection col1 && value2 instanceof Collection col2) {
            return arrayEquals(col1, col2, parentProperty);
        }

        if (value1 instanceof String && value2 instanceof String
                || value1 instanceof Number && value2 instanceof Number
                || value2 instanceof Boolean && value2 instanceof Boolean) {
            return value1.equals(value2);
        }

        return false;
    }

    private static final boolean objectEquals(
            final Map<?, ?> map1,
            final Map<?, ?> map2) {

        if (map1.size() != map2.size()) {
            return false;
        }

        if (map1.isEmpty()) {
            return true;
        }

        for (final var entry1 : map1.entrySet()) {

            final var prop2 = map2.get(entry1.getKey());
            if (!equals(
                    entry1.getValue(),
                    prop2,
                    entry1.getKey())) {
                return false;
            }
        }
        return true;
    }

    private static final boolean arrayEquals(
            final Collection<?> array1,
            final Collection<?> array2,
            final Object parentProperty) {

        if (array1.size() != array2.size()) {
            return false;
        }

        if (array1.isEmpty()) {
            return true;
        }

        // For values of @list, the order of these items is significant
        if (Keywords.LIST.equals(parentProperty)) {

            final var it1 = array1.iterator();
            final var it2 = array2.iterator();

            while (it1.hasNext() && it2.hasNext()) {
                if (!equals(it1.next(), it2.next())) {
                    return false;
                }
            }

            return !it1.hasNext() && !it2.hasNext();
        }

        return arraysEqualsUnordered(array1, array2);
    }

    // JSON arrays are generally compared with no regard to order
    private static final boolean arraysEqualsUnordered(
            final Collection<?> array1,
            final Collection<?> array2) {

        final var remaining = new ArrayList<>(array2);

        for (final var item1 : array1) {

            boolean found = false;

            for (final var item2 : remaining) {

                found = equals(item1, item2);

                if (found) {
                    remaining.remove(item2);
                    break;
                }
            }

            if (!found) {
                return false;
            }
        }

        return remaining.isEmpty();
    }

}
