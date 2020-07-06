package com.apicatalog.jsonld.lang;

import java.util.Arrays;

import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;

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
    public static final boolean isNodeObject(JsonValue value) {
        return JsonUtils.isObject(value)
                    && ((!value.asJsonObject().containsKey(Keywords.VALUE)
                                && !value.asJsonObject().containsKey(Keywords.LIST) 
                                && !value.asJsonObject().containsKey(Keywords.SET))
                            
                        || Arrays.asList(Keywords.CONTEXT, Keywords.GRAPH).containsAll(value.asJsonObject().keySet())
                        );
    }
    
    public static final boolean isNodeReference(JsonValue value) {
        return JsonUtils.isObject(value) 
                    && value.asJsonObject().size() == 1 
                    && value.asJsonObject().containsKey(Keywords.ID);        
    }
    
}
