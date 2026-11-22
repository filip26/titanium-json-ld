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

import java.math.BigDecimal;
import java.math.BigInteger;
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
 * it determines whether two nodes represent the same logical JSON-LD value.
 * </p>
 *
 * <h2>Comparison semantics</h2>
 * <ul>
 * <li>Scalars ({@code null}, {@code true}, {@code false}, and {@link String}
 * are compared by value.</li>
 * <li>Numeric scalar values ({@link Number}) are compared as big decimals.</li>
 * <li>{@link Map} is compared recursively by key and value. Key order is
 * ignored.</li>
 * <li>{@link Collection} is compared as an unordered collection, except when
 * the collection is the value of an {@code @list} property, in which case order
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
     * Compares two node for JSON-LD equality.
     *
     * @param a the first node to be compared
     * @param b the second node to be compared
     * @return {@code true} if both nodes are JSON-LD equivalent
     */
    public static final boolean equals(final Object a, final Object b) {
        return equals(a, b, null);
    }

    private static boolean equals(
            final Object a,
            final Object b,
            final Object parentProperty) {

        if (a == b) {
            return true;
        }

        if (a == null || b == null) {
            return false;
        }

        if (a instanceof Number numberA && b instanceof Number numberB) {
            return toBigDecimal(numberA).equals(toBigDecimal(numberB));
        }

        if (a instanceof Map mapA && b instanceof Map mapB) {
            return objectEquals(mapA, mapB);
        }

        if (a instanceof Collection colA && b instanceof Collection colB) {
            return arrayEquals(colA, colB, parentProperty);
        }

        if (a instanceof String && b instanceof String
                || b instanceof Boolean && b instanceof Boolean) {
            return a.equals(b);
        }

        return false;
    }

    private static boolean objectEquals(
            final Map<?, ?> mapA,
            final Map<?, ?> mapB) {

        if (mapA.size() != mapB.size()) {
            return false;
        }

        if (mapA.isEmpty()) {
            return true;
        }

        for (final var entryA : mapA.entrySet()) {
            if (!equals(
                    entryA.getValue(),
                    mapB.get(entryA.getKey()),
                    entryA.getKey())) {
                return false;
            }
        }
        return true;
    }

    private static final boolean arrayEquals(
            final Collection<?> arrayA,
            final Collection<?> arrayB,
            final Object parentProperty) {

        if (arrayA.size() != arrayB.size()) {
            return false;
        }

        if (arrayA.isEmpty()) {
            return true;
        }

        // For values of @list, the order of these items is significant
        if (Keywords.LIST.equals(parentProperty)) {

            final var itA = arrayA.iterator();
            final var itB = arrayB.iterator();

            while (itA.hasNext() && itB.hasNext()) {
                if (!equals(itA.next(), itB.next())) {
                    return false;
                }
            }

            return !itA.hasNext() && !itB.hasNext();
        }

        return arraysEqualsUnordered(arrayA, arrayB);
    }

    // JSON arrays are generally compared with no regard to order
    private static final boolean arraysEqualsUnordered(
            final Collection<?> arrayA,
            final Collection<?> arrayB) {

        final var remaining = new ArrayList<>(arrayB);

        for (final var itemA : arrayA) {

            boolean found = false;

            for (final var itemB : remaining) {

                found = equals(itemA, itemB);

                if (found) {
                    remaining.remove(itemB);
                    break;
                }
            }

            if (!found) {
                return false;
            }
        }

        return remaining.isEmpty();
    }

    private static BigDecimal toBigDecimal(Number number) {
        return switch (number) {
        case BigDecimal bd -> bd;
        case BigInteger bi -> new BigDecimal(bi);
        case Float f -> BigDecimal.valueOf(f.doubleValue());
        case Double d -> BigDecimal.valueOf(d);
        case Long l -> BigDecimal.valueOf(l);
        case Integer i -> BigDecimal.valueOf(i);
        case Short s -> BigDecimal.valueOf(s);
        case Byte b -> BigDecimal.valueOf(b);
        default -> new BigDecimal(number.toString());
        };
    }
}
