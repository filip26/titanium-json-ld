package com.apicatalog.jsonld.lang;

import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;

public final class ValueObject {

    private ValueObject() {
    }

    public static final boolean isValueObject(JsonValue value) {
        return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.VALUE);
    }

    public static JsonValue getValue(JsonValue value) {
        return isValueObject(value)
                    ? value.asJsonObject().get(Keywords.VALUE)
                    : null; 
    }

}
