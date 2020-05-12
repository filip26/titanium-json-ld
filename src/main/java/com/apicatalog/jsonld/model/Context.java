package com.apicatalog.jsonld.model;

import java.util.LinkedHashMap;
import java.util.Map;

public final class Context {

	private final Map<PropertyName, PropertyValue> map;
	
	protected Context() {
		this.map = new LinkedHashMap<>();
	}
	
	
}
