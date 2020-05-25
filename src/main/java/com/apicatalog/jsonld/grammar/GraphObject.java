package com.apicatalog.jsonld.grammar;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.utils.JsonUtils;

public class GraphObject {

    GraphObject() {
    }

    public static final boolean isGraphObject(JsonValue value) {
        return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.GRAPH);
    }

    public static final JsonObject toGraphObject(JsonValue value) {
        return Json.createObjectBuilder().add(Keywords.GRAPH, JsonUtils.toJsonArray(value)).build();
    }

}
