package com.apicatalog.jsonld.grammar;

import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

public final class ValueObject {

	ValueObject() {
	}
	
	
	public static final boolean isValueObject(JsonValue value) {
		return ValueType.OBJECT.equals(value.getValueType()) && value.asJsonObject().containsKey(Keywords.VALUE);		
	}
	
}
