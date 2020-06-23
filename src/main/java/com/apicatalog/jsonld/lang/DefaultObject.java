package com.apicatalog.jsonld.lang;

import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;

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
    public static final boolean isDefaultObject(JsonValue value) {
        return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.DEFAULT);
    }

    public static JsonValue getValue(JsonValue value) {
        return JsonUtils.isObject(value) ? value.asJsonObject().get(Keywords.DEFAULT) : null;
    }

}
