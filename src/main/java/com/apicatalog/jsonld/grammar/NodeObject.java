package com.apicatalog.jsonld.grammar;

import javax.json.JsonValue;

import com.apicatalog.jsonld.utils.JsonUtils;

public class NodeObject {

    NodeObject() {
    }

    public static final boolean isNodeObject(JsonValue value) {
        return JsonUtils.isObject(value) && ((!value.asJsonObject().containsKey(Keywords.VALUE)
                && !value.asJsonObject().containsKey(Keywords.LIST) && !value.asJsonObject().containsKey(Keywords.SET))
                || (value.asJsonObject().size() == 2 && value.asJsonObject().containsKey(Keywords.GRAPH))
                        && value.asJsonObject().containsKey(Keywords.CONTEXT))

        ;
        // TODO https://www.w3.org/TR/json-ld11/#dfn-node-object
    }

}
