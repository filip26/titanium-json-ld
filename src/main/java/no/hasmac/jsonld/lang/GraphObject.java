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
import java.util.HashSet;
import java.util.Set;

import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11/#graph-objects">Graph Objects</a>
 *
 */
public final class GraphObject {

    private GraphObject() {
    }

    public static boolean isGraphObject(JsonValue value) {
        if (!JsonUtils.isObject(value) || !value.asJsonObject().containsKey(Keywords.GRAPH)) {
            return false;
        }
        Set<String> allowed = new HashSet<>(Arrays.asList(Keywords.GRAPH, Keywords.ID, Keywords.INDEX, Keywords.CONTEXT));

        return allowed.containsAll(value.asJsonObject().keySet());
    }

    public static boolean isSimpleGraphObject(JsonValue value) {

        return isGraphObject(value) && !value.asJsonObject().containsKey(Keywords.ID);
    }

    public static JsonObject toGraphObject(JsonValue value) {
        return JsonProvider.instance().createObjectBuilder().add(Keywords.GRAPH, JsonUtils.toJsonArray(value)).build();
    }

}
