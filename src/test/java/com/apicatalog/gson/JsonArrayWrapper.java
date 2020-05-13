package com.apicatalog.gson;

import java.util.Iterator;

import com.apicatalog.json.JsonArray;
import com.apicatalog.json.JsonElement;

public class JsonArrayWrapper implements JsonArray {

	private final com.google.gson.JsonArray jsonArray;
	
	public JsonArrayWrapper(final com.google.gson.JsonArray jsonArray) {
		this.jsonArray = jsonArray;
	}
	
	@Override
	public Iterator<JsonElement> iterator() {

		final Iterator<com.google.gson.JsonElement> iterator = jsonArray.iterator();
		
		return new Iterator<JsonElement>() {

			@Override
			public boolean hasNext() {
				return iterator.hasNext();
			}

			@Override
			public JsonElement next() {
				
				final com.google.gson.JsonElement jsonElement = iterator.next();
				
				return jsonElement != null ? new JsonElementWrapper(jsonElement) : null;
			}
		};
	}

	@Override
	public int size() {
		return jsonArray.size();
	}

}
