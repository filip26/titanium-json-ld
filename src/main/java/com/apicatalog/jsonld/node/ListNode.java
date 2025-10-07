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
package com.apicatalog.jsonld.node;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public final class ListNode {

    private ListNode() {
    }

    /**
     * A list object is a map that has a @list key. It may also have an @index key,
     * but no other entries. See the Lists and Sets section of JSON-LD 1.1 for a
     * normative description.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-list-object">List
     *      Object</a>
     *
     * @param value to check
     * @return <code>true</code> if the provided value is valid list object
     */
    public static final boolean isListNode(JsonValue value) {
        return JsonUtils.containsKey(value, Keywords.LIST)
                && (value.asJsonObject().size() == 1
                        || (value.asJsonObject().size() == 2
                                && value.asJsonObject().containsKey(Keywords.INDEX)));
    }

    public static final boolean isList(Object value) {
        if (value instanceof Map map) {
            return map.containsKey(Keywords.LIST)
                    && (map.size() == 1
                            || map.size() == 2
                                    && map.containsKey(Keywords.INDEX));
        }
        return false;
    }

    /**
     * Convert expanded value to a list object by first setting it to an array
     * containing only expanded value if it is not already an array, and then by
     * setting it to a map containing the key-value pair @list-expanded value.
     *
     * @param value to convert
     * @return list object containing the provided value
     */
    public static final JsonObject toListNode(JsonValue value) {
        if (JsonUtils.isArray(value)) {
            return JsonProvider.instance().createObjectBuilder().add(Keywords.LIST, value).build();
        }

        return JsonProvider.instance().createObjectBuilder().add(Keywords.LIST, JsonProvider.instance().createArrayBuilder().add(value)).build();
    }
    
    public static final Map<String, ?> toList(Object value) {
        if (value instanceof Collection) {
            return Map.of(Keywords.LIST, value);
        }
        return Map.of(Keywords.LIST, Set.of(value));
    }
}
