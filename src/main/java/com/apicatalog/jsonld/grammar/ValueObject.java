package com.apicatalog.jsonld.grammar;

import javax.json.JsonValue;

import com.apicatalog.jsonld.utils.JsonUtils;

public final class ValueObject {

    ValueObject() {
    }

    public static final boolean isValueObject(JsonValue value) {
        return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.VALUE);
    }

}
