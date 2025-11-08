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
import java.util.Objects;

import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;

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
     * Compares two nodes using the same {@link TreeAdapter}.
     *
     * @return {@code true} if both nodes are JSON-LD equivalent
     */
    public static final boolean equals(
            final Object value1,
            final Object value2,
            final TreeAdapter adapter) {

        return equals(value1, adapter, value2, adapter, null);
    }

    /**
     * Compares two nodes that may use different {@link TreeAdapter}s.
     *
     * @return {@code true} if both nodes are JSON-LD equivalent
     */
    public static final boolean equals(
            final Object value1, final TreeAdapter adapter1,
            final Object value2, final TreeAdapter adapter2) {

        return equals(value1, adapter1, value2, adapter2, null);
    }

    private static final boolean equals(
            final Object value1, final TreeAdapter adapter1,
            final Object value2, final TreeAdapter adapter2,
            final String parentProperty) {

        final var type1 = adapter1.type(value1);

        if (type1 == NodeType.TREE_IO) {

            return equals(
                    ((TreeIO) value1).node(), ((TreeIO) value1).adapter(),
                    value2, adapter2,
                    parentProperty);
        }

        final var type2 = adapter2.type(value2);

        if (type2 == NodeType.TREE_IO) {
            return equals(
                    value1, adapter1,
                    ((TreeIO) value2).node(), ((TreeIO) value2).adapter(),
                    parentProperty);
        }

        if (type1 != type2) {
            return false;
        }

        final var nativeEquals = adapter1.isCompatibleWith(adapter2);

        return switch (type1) {
        case NULL, TRUE, FALSE -> true;

        case STRING -> nativeEquals
                ? Objects.equals(value1, value2)
                : Objects.equals(
                        adapter1.stringValue(value1),
                        adapter2.stringValue(value2));

        case NUMBER -> nativeEquals
                ? Objects.equals(value1, value2)
                : Objects.equals(
                        adapter1.numericValue(value1),
                        adapter2.numericValue(value2));

        case BINARY -> nativeEquals
                ? Objects.equals(value1, value2)
                : Objects.equals(
                        adapter1.binaryValue(value1),
                        adapter2.binaryValue(value2));

        case COLLECTION -> arrayEquals(value1, adapter1, value2, adapter2, parentProperty);

        case MAP -> objectEquals(value1, adapter1, value2, adapter2);

        case TREE_IO -> throw new IllegalStateException();

        default -> false;

        };
    }

    private static final boolean objectEquals(
            final Object map1, final TreeAdapter adapter1,
            final Object map2, final TreeAdapter adapter2) {

        final var it1 = adapter1.entryStream(map1).toList();

        if (it1.size() != adapter2.keyStream(map2).count()) {
            return false;
        }

        for (final var entry1 : it1) {

            final var prop2 = adapter2.property(entry1.getKey(), adapter1, map2);
            if (!equals(
                    entry1.getValue(), adapter1,
                    prop2, adapter2,
                    adapter1.asString(entry1.getKey()))) {
                return false;
            }
        }
        return true;
    }

    private static final boolean arrayEquals(
            final Object array1, final TreeAdapter adapter1,
            final Object array2, final TreeAdapter adapter2,
            final String parentProperty) {

        // For values of @list, the order of these items is significant
        if (Keywords.LIST.equals(parentProperty)) {

            final var it1 = adapter1.elements(array1).iterator();
            final var it2 = adapter2.elements(array2).iterator();

            while (it1.hasNext() && it2.hasNext()) {
                if (!equals(it1.next(), adapter1, it2.next(), adapter2)) {
                    return false;
                }
            }

            return !it1.hasNext() && !it2.hasNext();
        }

        return arraysEqualsUnordered(array1, adapter1, array2, adapter2);
    }

    // JSON arrays are generally compared with no regard to order
    private static final boolean arraysEqualsUnordered(
            final Object array1, final TreeAdapter adapter1,
            final Object array2, final TreeAdapter adapter2) {

        var list1 = adapter1.elementStream(array1).toList();
        var list2 = adapter2.elementStream(array2).toList();

        if (list1.size() != list2.size()) {
            return false;
        }

        if (list1.isEmpty()) {
            return true;
        }

        final var remaining = new ArrayList<>(list2);

        for (final var item1 : list1) {

            boolean found = false;

            for (final var item2 : remaining) {

                found = equals(item1, adapter1, item2, adapter2);

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
