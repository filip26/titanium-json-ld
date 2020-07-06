package com.apicatalog.jsonld.lang;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11/#graph-objects">Graph Objects</a>
 *
 */
public final class GraphObject {

    private GraphObject() {
    }

    public static final boolean isGraphObject(JsonValue value) {
        if (!JsonUtils.isObject(value) || !value.asJsonObject().containsKey(Keywords.GRAPH)) {
            return false;
        }
        Set<String> allowed = new HashSet<>(Arrays.asList(Keywords.GRAPH, Keywords.ID, Keywords.INDEX, Keywords.CONTEXT));

        return allowed.containsAll(value.asJsonObject().keySet());
    }
    
    public static final boolean isSimpleGraphObject(JsonValue value) {

        return isGraphObject(value) && !value.asJsonObject().containsKey(Keywords.ID);        
    }
    
    public static final JsonObject toGraphObject(JsonValue value) {
        return Json.createObjectBuilder().add(Keywords.GRAPH, JsonUtils.toJsonArray(value)).build();
    }

}
