package com.apicatalog.jsonld.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.tree.io.TreeIOAdapter;

public class LdAdapter {

    private static final Set<String> GRAPH_ALLOWED_KEYS = Set.of(
            Keywords.GRAPH,
            Keywords.ID,
            Keywords.INDEX,
            Keywords.CONTEXT);

    private static final Collection<String> VALUE_KEYWORDS = Arrays.asList(
            Keywords.TYPE,
            Keywords.VALUE,
            Keywords.DIRECTION,
            Keywords.LANGUAGE,
            Keywords.INDEX,
            Keywords.ANNOTATION);

    /**
     * Check if the given value is valid node object.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-node-object">Node
     *      Object</a>
     *
     * @param node to check
     * @return <code>true</code> if the provided value is valid node object
     */
    public static final boolean isNode(Object node) {
        return node instanceof Map map
                && ((!map.containsKey(Keywords.VALUE)
                        && !map.containsKey(Keywords.LIST)
                        && !map.containsKey(Keywords.SET))
                        || Arrays.asList(Keywords.CONTEXT, Keywords.GRAPH).containsAll(map.keySet()));
    }

    public static boolean isNode(Object value, TreeIOAdapter adapter) {
        if (!adapter.isMap(value)) {
            return false;
        }
        return adapter.keyStream(value).noneMatch(Set.of(
                Keywords.VALUE,
                Keywords.LIST,
                Keywords.SET)::contains)
                || Set.of(
                        Keywords.CONTEXT,
                        Keywords.GRAPH).containsAll(adapter.keys(value));
    }

    public static final boolean isReference(Object node) {
        return node instanceof Map map
                && map.size() == 1
                && map.containsKey(Keywords.ID);
    }

    public static final boolean isGraph(Object node) {
        return node instanceof Map map
                && map.containsKey(Keywords.GRAPH)
                && GRAPH_ALLOWED_KEYS.containsAll(map.keySet());
    }

    public static final boolean isSimpleGraph(Map<?, ?> node) {
        return isGraph(node) && !node.containsKey(Keywords.ID);
    }

    /**
     * @see <a href="https://www.w3.org/TR/json-ld11/#graph-objects">Graph
     *      Objects</a>
     */
    public static boolean isNotGraph(Object node) {
        return node == null
                || !(node instanceof Map map)
                || !map.keySet().contains(Keywords.GRAPH)
                || !GRAPH_ALLOWED_KEYS.containsAll(map.keySet());
    }

    /**
     * A default node is a map that has a @default key.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-default-object">Default
     *      Object</a>
     *
     * @param value to check
     * @return <code>true</code> if the provided value is valid default object
     */
    public static boolean isDefault(Object node) {
        return node instanceof Map map && map.keySet().contains(Keywords.DEFAULT);
    }

    /**
     * A default node is a map that has a @default key.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-default-object">Default
     *      Object</a>
     *
     * @param value   to check
     * @param adapter
     * @return <code>true</code> if the provided value is valid default object
     */
    public static boolean isDefault(Object node, TreeIOAdapter adapter) {
        return adapter.isMap(node) && adapter.keys(node).contains(Keywords.DEFAULT);
    }

    public static Optional<?> findDefaultValue(Object node, TreeIOAdapter adapter) {
        return adapter.isMap(node)
                ? Optional.ofNullable(adapter.property(Keywords.DEFAULT, node))
                : Optional.empty();
    }

    /**
     * A list object is a map that has a @list key. It may also have an @index key,
     * but no other entries. See the Lists and Sets section of JSON-LD 1.1 for a
     * normative description.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-list-object">List
     *      Object</a>
     *
     * @param node to check
     * @return <code>true</code> if the provided value is valid list object
     */
    public static boolean isList(Map<?, ?> node) {
        return node.containsKey(Keywords.LIST)
                && (node.size() == 1
                        || node.size() == 2
                                && node.containsKey(Keywords.INDEX));
    }

//    public static boolean isList(Object node) {
//        return node instanceof Map map && isList(map);
//    }
//

    public static Map<String, ?> toList(Object node) {
        return node instanceof Collection
                ? Map.of(Keywords.LIST, node)
                : Map.of(Keywords.LIST, Set.of(node));
    }

    public static boolean isValueNode(Map<?, ?> node) {
        return node.containsKey(Keywords.VALUE);
    }

    public static boolean isNotValueNode(Object node) {
        return !(node instanceof Map map && VALUE_KEYWORDS.containsAll(map.keySet()));
    }

    public static Optional<Object> findValue(Object node) {
        return (node instanceof Map<?, ?> map)
                ? Optional.ofNullable(map.get(Keywords.VALUE))
                : Optional.empty();

//        return isValueObject(node)
//                ? Optional.ofNullable(node.asJsonObject().get(Keywords.VALUE))
//                : Optional.empty();
    }

    /* --- */

    public static void setOrAdd(Map<String, Object> result, String key, Object value) {
        setOrAdd(result, key, value, true);
    }

//    public static Map<String, Object> setOrAdd(Map<String, Object> result, String parent, String key, Object value, boolean asArray) {
//    
//        Object node = result.computeIfAbsent(parent, k -> new LinkedHashMap<String, Object>());
//        
//        if (!node instanceof Map) {
//            node = new 
//        }
//        
//        JsonLdAdapter.setOrAdd(
//            (Map<String, Object>) result.computeIfAbsent(parent, k -> new LinkedHashMap<String, Object>()),
//            mapKey,
//            compactedItem,
//            asArray);
//    
//        return result;
//    }

    public static void setOrAdd(
            final Map<String, Object> source,
            final String key, Object value,
            final boolean asArray) {
//
//        // 1. If as array is true and the value of key in object does not exist or is
//        // not an array,
//        // set it to a new array containing any original value.
//        if (asArray) {
//
//            var original = source.get(key);
//
//            if (original == null) {
//                source.put(key, new ArrayList<>());
//
//            } else if (!(original instanceof Collection)) {
//                var x = new ArrayList<>();
//                x.add(original);
//                source.put(key, x);
//            }
//        }
//
//        // 2. If value is an array, then for each element v in value, use add value
//        // recursively to add v to key in entry.
//        if (value instanceof Collection<?> array) {
//            array.forEach(v -> setOrAdd(source, key, v, asArray));
//
//            // 3.
//        } else {
//
//            final var original = source.get(key);
//
//            // 3.1
//            if (original != null) {
//
//                if (original instanceof Collection array) {
//                    array.add(value);
//
//                } else {
//                    var x = new ArrayList<>();
//                    x.add(original);
//                    x.add(value);
//                    source.put(key, x);
//                }
//
//                // 3.2
//            } else {
//                source.put(key, value);
//            }
//        }

        var previous = source.get(key);

        if (previous == null) {
            if (value instanceof Collection<?> array) {
                if (!asArray && array.size() == 1) {
                    source.put(key, array.iterator().next());
                    return;
                }
                source.put(key, value);
                return;
            }
            if (asArray) {
                if (value == null) {
                    source.put(key, List.of());
                    return;
                }
                source.put(key, List.of(value));
                return;
            }
            source.put(key, value);
            return;
        }

        final Collection<Object> array;

        if (previous instanceof ArrayList list) {

            if (!asArray && list.isEmpty()) {
                if (value instanceof Collection<?> single && single.size() == 1) {
                    source.put(key, single.iterator().next());
                    return;
                }

                source.put(key, value);
                return;
            }

            array = list;

        } else if (previous instanceof Collection<?> col) {

            if (!asArray && col.isEmpty()) {
                if (value instanceof Collection<?> single && single.size() == 1) {
                    source.put(key, single.iterator().next());
                    return;
                }

                source.put(key, value);
                return;
            }

            array = new ArrayList<Object>(col);
            source.put(key, array);

        } else {
            array = new ArrayList<>();
            array.add(previous);
            source.put(key, array);
        }

        if (value instanceof Collection<?> col) {            
            array.addAll(col);
        } else {
            array.add(value);
        }
    }

    /* ---- TODO ---- */
//    @Deprecated
//    public static final boolean isNodeJakarta(JsonValue value) {
//        return JsonUtils.isObject(value)
//                && ((!value.asJsonObject().containsKey(Keywords.VALUE)
//                        && !value.asJsonObject().containsKey(Keywords.LIST)
//                        && !value.asJsonObject().containsKey(Keywords.SET))
//
//                        || Arrays.asList(Keywords.CONTEXT, Keywords.GRAPH).containsAll(value.asJsonObject().keySet()));
//    }
//
//    @Deprecated
//    public static final boolean isNotNodeJakarta(JsonValue value) {
//        return !isNodeJakarta(value);
//    }
//
//    @Deprecated
//    public static final boolean isReferenceJakarta(JsonValue value) {
//        return JsonUtils.containsKey(value, Keywords.ID) && value.asJsonObject().size() == 1;
//    }

    // Extension: JSON-LD-STAR (Experimental)
//    @Deprecated
//    public static final boolean isEmbedded(JsonValue value) {
//
//        if (JsonUtils.isNotObject(value)) {
//            return false;
//        }
//
//        final JsonObject node = value.asJsonObject();
//
//        boolean found = false;
//
//        for (Entry<String, JsonValue> property : node.entrySet()) {
//
//            if (property.getKey().equals(Keywords.INDEX)
//                    || property.getKey().equals(Keywords.CONTEXT)
//                    || property.getKey().equals(Keywords.REVERSE)) {
//                return false;
//            }
//
//            if (!Keywords.TYPE.equals(property.getKey()) && Keywords.matchForm(property.getKey())) {
//                continue;
//            }
//
//            // validate property name
//            if (!found && (Keywords.TYPE.equals(property.getKey()) || UriUtils.isURI(property.getKey()))) {
//
//                // validate property value
//                JsonValue propertyValue = property.getValue();
//
//                if (JsonUtils.isArray(propertyValue)) {
//
//                    if (propertyValue.asJsonArray().size() != 1) {
//                        return false;
//                    }
//
//                    propertyValue = propertyValue.asJsonArray().get(0);
//                }
//
//                if (isValueObject(propertyValue)) {
//                    propertyValue = getValueObject(propertyValue).orElse(null);
//                }
//
//                if (JsonUtils.isString(propertyValue)
//                        || (JsonUtils.isObject(propertyValue) && isEmbedded(propertyValue.asJsonObject()))) {
//                    found = true;
//                    continue;
//                }
//            }
//            return false;
//        }
//
//        return true;
//    }
//
//    // Extension: JSON-LD-STAR (Experimental)
//    @Deprecated
//    public static final boolean isNotAnnotation(final JsonValue annotation) {
//        return !isAnnotation(annotation);
//    }
//
//    @Deprecated
//    public static final boolean isAnnotation(final JsonValue annotation) {
//
//        JsonValue value = annotation;
//
//        if (JsonUtils.isArray(value)) {
//            return value.asJsonArray().stream().allMatch(JsonLdAdapter::isAnnotation);
//        }
//
//        if (JsonUtils.isNotObject(value)) {
//            return false;
//        }
//
//        for (Entry<String, JsonValue> property : value.asJsonObject().entrySet()) {
//
//            if (Keywords.ANNOTATION.equals(property.getKey()) && !isAnnotation(property.getValue())) {
//                return false;
//            }
//
//            if (Keywords.matchForm(property.getKey()) && !Keywords.TYPE.equals(property.getKey()) && !Keywords.REVERSE.equals(property.getKey())) {
//                return false;
//            }
//
//        }
//
//        return true;
//    }
//
//    @Deprecated
//    public static final boolean isListObject(JsonValue value) {
//        return JsonUtils.containsKey(value, Keywords.LIST)
//                && (value.asJsonObject().size() == 1
//                        || (value.asJsonObject().size() == 2
//                                && value.asJsonObject().containsKey(Keywords.INDEX)));
//    }
//
//    /**
//     * Convert expanded value to a list object by first setting it to an array
//     * containing only expanded value if it is not already an array, and then by
//     * setting it to a map containing the key-value pair @list-expanded value.
//     *
//     * @param value to convert
//     * @return list object containing the provided value
//     */
//    @Deprecated
//    public static final JsonObject toListObject(JsonValue value) {
//        if (JsonUtils.isArray(value)) {
//            return JsonProvider.instance().createObjectBuilder().add(Keywords.LIST, value).build();
//        }
//
//        return JsonProvider.instance().createObjectBuilder().add(Keywords.LIST, JsonProvider.instance().createArrayBuilder().add(value)).build();
//    }
//
//    @Deprecated
//    public static final boolean isValueObject(JsonValue value) {
//        return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.VALUE);
//    }
//
//    @Deprecated
//    public static Optional<JsonValue> getValueObject(JsonValue value) {
//        return isValueObject(value)
//                ? Optional.ofNullable(value.asJsonObject().get(Keywords.VALUE))
//                : Optional.empty();
//    }

}
