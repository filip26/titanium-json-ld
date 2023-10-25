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
package no.hasmac.jsonld.lang;

import java.util.Arrays;
import java.util.Map.Entry;

import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.uri.UriUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public final class NodeObject {

    private NodeObject() {
    }

    /**
     * Check if the given value is valid node object.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-node-object">Node Object</a>
     *
     * @param value to check
     * @return <code>true</code> if the provided value is valid node object
     */
    public static boolean isNodeObject(JsonValue value) {
        return JsonUtils.isObject(value)
                    && ((!value.asJsonObject().containsKey(Keywords.VALUE)
                                && !value.asJsonObject().containsKey(Keywords.LIST)
                                && !value.asJsonObject().containsKey(Keywords.SET))

                        || Arrays.asList(Keywords.CONTEXT, Keywords.GRAPH).containsAll(value.asJsonObject().keySet())
                        );
    }

    public static boolean isNotNodeObject(JsonValue value) {
        return !isNodeObject(value);
    }

    public static boolean isNodeReference(JsonValue value) {
        return JsonUtils.containsKey(value, Keywords.ID) && value.asJsonObject().size() == 1;
    }

    // Extension: JSON-LD-STAR (Experimental)
    public static boolean isEmbeddedNode(JsonValue value) {

        if (JsonUtils.isNotObject(value)) {
            return false;
        }

        final JsonObject node = value.asJsonObject();

        boolean found = false;

        for (Entry<String, JsonValue> property : node.entrySet()) {

            if (property.getKey().equals(Keywords.INDEX)
                || property.getKey().equals(Keywords.CONTEXT)
                || property.getKey().equals(Keywords.REVERSE)
                ) {
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

                if (ValueObject.isValueObject(propertyValue)) {
                    propertyValue = ValueObject.getValue(propertyValue).orElse(null);
                }

                if (JsonUtils.isString(propertyValue)
                        || (JsonUtils.isObject(propertyValue) && isEmbeddedNode(propertyValue.asJsonObject()))
                        ) {
                    found = true;
                    continue;
                }
            }
            return false;
        }

        return true;
    }

    // Extension: JSON-LD-STAR (Experimental)
    public static boolean  isNotAnnotationObject(final JsonValue annotation) {
        return !isAnnotationObject(annotation);
    }

    public static boolean  isAnnotationObject(final JsonValue annotation) {

        JsonValue value = annotation;

        if (JsonUtils.isArray(value)) {
            return value.asJsonArray().stream().allMatch(NodeObject::isAnnotationObject);
        }

        if (JsonUtils.isNotObject(value)) {
            return false;
        }

        for (Entry<String, JsonValue> property : value.asJsonObject().entrySet()) {

            if (Keywords.ANNOTATION.equals(property.getKey()) && !isAnnotationObject(property.getValue())) {
                return false;
            }

            if (Keywords.matchForm(property.getKey()) && !Keywords.TYPE.equals(property.getKey()) && !Keywords.REVERSE.equals(property.getKey())) {
                return false;
            }

        }

        return true;
    }
}
