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

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonValue;

public final class ValueNode {

    private static final Collection<String> VALUE_KEYWORDS = Arrays.asList(
            Keywords.TYPE,
            Keywords.VALUE,
            Keywords.DIRECTION,
            Keywords.LANGUAGE,
            Keywords.INDEX,
            Keywords.ANNOTATION);

    
    public static final boolean isValueObject(JsonValue value) {
        return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.VALUE);
    }

    public static Optional<JsonValue> getValue(JsonValue value) {
        return isValueObject(value)
                ? Optional.ofNullable(value.asJsonObject().get(Keywords.VALUE))
                : Optional.empty();
    }

    public static final boolean isValueNode(Object value) {
        return value instanceof Map map 
                && map.containsKey(Keywords.VALUE);
    }
    
    public static final boolean isNotValueNode(Object value) {
        return !(value instanceof Map map && VALUE_KEYWORDS.containsAll(map.keySet()));
    }
}
