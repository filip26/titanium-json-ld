package com.apicatalog.jsonld.grammar;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11/#graph-objects">Graph Objects</a>
 *
 */
public class GraphObject {

    GraphObject() {
    }

    public static final boolean isGraphObject(JsonValue value) {
        if (!JsonUtils.isObject(value) || !value.asJsonObject().containsKey(Keywords.GRAPH)) {
            return false;
        }
        Set<String> allowed = new HashSet<>(Arrays.asList(Keywords.GRAPH, Keywords.ID, Keywords.INDEX, Keywords.CONTEXT));

        return value.asJsonObject().keySet().stream().allMatch(allowed::contains);
    }
    
    public static final boolean isSimpleGraphObject(JsonValue value) {
        return isGraphObject(value) && !value.asJsonObject().containsKey(Keywords.ID);        
    }
    
    public static final JsonObject toGraphObject(JsonValue value) {
        return Json.createObjectBuilder().add(Keywords.GRAPH, JsonUtils.toJsonArray(value)).build();
    }

}
