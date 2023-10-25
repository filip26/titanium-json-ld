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

import java.util.Optional;

import no.hasmac.jsonld.json.JsonUtils;

import jakarta.json.JsonValue;

public final class DefaultObject {

    private DefaultObject() {
    }

    /**
     * A default object is a map that has a @default key.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-default-object">Default
     *      Object</a>
     *
     * @param value to check
     * @return <code>true</code> if the provided value is valid default object
     */
    public static boolean isDefaultObject(JsonValue value) {
        return JsonUtils.containsKey(value, Keywords.DEFAULT);
    }

    public static Optional<JsonValue> getValue(JsonValue value) {
        return JsonUtils.isObject(value)
                            ? Optional.ofNullable(value.asJsonObject().get(Keywords.DEFAULT))
                            : Optional.empty();
    }

}
