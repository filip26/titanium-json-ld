package com.apicatalog.jsonld.lang;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Map.Entry;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.tree.io.NodeAdapter;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public class JsonLdNode {

    private static final Set<String> GRAPH_ALLOWED_KEYS = Set.of(
            Keywords.GRAPH,
            Keywords.ID,
            Keywords.INDEX,
            Keywords.CONTEXT);

    /**
     * @see <a href="https://www.w3.org/TR/json-ld11/#graph-objects">Graph
     *      Objects</a>
     */
    public static final boolean isNotGraphNode(Object value) {
        return value == null
                || !(value instanceof Map map)
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
    public static final boolean isDefaultNode(Object node, NodeAdapter adapter) {
        return adapter.isMap(node) && adapter.keys(node).contains(Keywords.DEFAULT);
    }

    public static Optional<?> findDefaultValue(Object node, NodeAdapter adapter) {
        return adapter.isMap(node)
                ? Optional.ofNullable(adapter.property(Keywords.DEFAULT, node))
                : Optional.empty();
    }

    
    
    /**
     * Check if the given value is valid node object.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-node-object">Node
     *      Object</a>
     *
     * @param value to check
     * @return <code>true</code> if the provided value is valid node object
     */
    public static final boolean isNode(JsonValue value) {
        return JsonUtils.isObject(value)
                && ((!value.asJsonObject().containsKey(Keywords.VALUE)
                        && !value.asJsonObject().containsKey(Keywords.LIST)
                        && !value.asJsonObject().containsKey(Keywords.SET))

                        || Arrays.asList(Keywords.CONTEXT, Keywords.GRAPH).containsAll(value.asJsonObject().keySet()));
    }

    public static final boolean isNode(Object value, NodeAdapter adapter) {
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

    public static final boolean isNotNode(JsonValue value) {
        return !isNode(value);
    }

    public static final boolean isReference(JsonValue value) {
        return JsonUtils.containsKey(value, Keywords.ID) && value.asJsonObject().size() == 1;
    }

    // Extension: JSON-LD-STAR (Experimental)
    public static final boolean isEmbedded(JsonValue value) {

        if (JsonUtils.isNotObject(value)) {
            return false;
        }

        final JsonObject node = value.asJsonObject();

        boolean found = false;

        for (Entry<String, JsonValue> property : node.entrySet()) {

            if (property.getKey().equals(Keywords.INDEX)
                    || property.getKey().equals(Keywords.CONTEXT)
                    || property.getKey().equals(Keywords.REVERSE)) {
                return false;
            }

            if (!Keywords.TYPE.equals(property.getKey()) && Keywords.matchForm(property.getKey())) {
                continue;
            }

            // validate property name
            if (!found && (Keywords.TYPE.equals(property.getKey()) || UriUtils.isURI(property.getKey()))) {

                // validate property value
                JsonValue propertyValue = property.getValue();

                if (JsonUtils.isArray(propertyValue)) {

                    if (propertyValue.asJsonArray().size() != 1) {
                        return false;
                    }

                    propertyValue = propertyValue.asJsonArray().get(0);
                }

                if (ValueNode.isValueObject(propertyValue)) {
                    propertyValue = ValueNode.getValue(propertyValue).orElse(null);
                }

                if (JsonUtils.isString(propertyValue)
                        || (JsonUtils.isObject(propertyValue) && isEmbedded(propertyValue.asJsonObject()))) {
                    found = true;
                    continue;
                }
            }
            return false;
        }

        return true;
    }

    // Extension: JSON-LD-STAR (Experimental)
    public static final boolean isNotAnnotation(final JsonValue annotation) {
        return !isAnnotation(annotation);
    }

    public static final boolean isAnnotation(final JsonValue annotation) {

        JsonValue value = annotation;

        if (JsonUtils.isArray(value)) {
            return value.asJsonArray().stream().allMatch(JsonLdNode::isAnnotation);
        }

        if (JsonUtils.isNotObject(value)) {
            return false;
        }

        for (Entry<String, JsonValue> property : value.asJsonObject().entrySet()) {

            if (Keywords.ANNOTATION.equals(property.getKey()) && !isAnnotation(property.getValue())) {
                return false;
            }

            if (Keywords.matchForm(property.getKey()) && !Keywords.TYPE.equals(property.getKey()) && !Keywords.REVERSE.equals(property.getKey())) {
                return false;
            }

        }

        return true;
    }
    
}
