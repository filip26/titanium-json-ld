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
package com.apicatalog.jsonld.json;

import java.util.ArrayList;
import java.util.Objects;

import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.PolyNode;

/**
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-api/tests/#json-ld-object-comparison">JSON-LD
 *      Object comparison</a>
 *
 */
public final class JsonLdComparison {

    private JsonLdComparison() {
    }

    public static final boolean equals(
            final Object value1,
            final Object value2,
            final NodeAdapter adapter) {

        return equals(value1, adapter, value2, adapter, null);
    }

    public static final boolean equals(
            final Object value1, final NodeAdapter adapter1,
            final Object value2, final NodeAdapter adapter2) {

        return equals(value1, adapter1, value2, adapter2, null);
    }

    private static final boolean equals(
            final Object value1, final NodeAdapter adapter1,
            final Object value2, final NodeAdapter adapter2,
            final String parentProperty) {

        if (value1 == null) {
            return value2 != null;

        } else if (value2 == null) {
            return false;
        }

        final var type1 = adapter1.type(value1);

        if (type1 == NodeType.POLY) {
            return equals(
                    ((PolyNode) value1).node(), ((PolyNode) value1).adapter(),
                    value2, adapter2,
                    parentProperty);
        }

        final var type2 = adapter2.type(value2);

        if (type2 == NodeType.POLY) {
            return equals(
                    value1, adapter1,
                    ((PolyNode) value2).node(), ((PolyNode) value2).adapter(),
                    parentProperty);
        }

        if (type1 != type2) {
            return false;
        }

        boolean nativeEquals = adapter1.isCompatibleWith(adapter2);

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

        case POLY -> throw new IllegalStateException();

        default -> false;

        };
    }

    private static final boolean objectEquals(
            final Object map1, final NodeAdapter adapter1,
            final Object map2, final NodeAdapter adapter2) {

        final var it1 = adapter1.entries(map1);

        for (final var entry1 : it1) {

            final var prop2 = adapter2.property(entry1.getKey(), adapter1, map2);

            if (prop2 == null && entry1.getValue() != null
                    || prop2 != null && entry1.getValue() == null) {
                return false;
            }

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
            final Object array1, final NodeAdapter adapter1,
            final Object array2, final NodeAdapter adapter2,
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
            final Object array1, final NodeAdapter adapter1,
            final Object array2, final NodeAdapter adapter2) {

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
