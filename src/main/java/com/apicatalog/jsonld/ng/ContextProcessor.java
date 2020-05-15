package com.apicatalog.jsonld.ng;

import java.net.URL;
import java.util.ArrayDeque;
import java.util.Deque;

import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdContext;

//TODO the future 
public class ContextProcessor {

	private final Deque<Context> contexts;
	
	public ContextProcessor() {
		this.contexts = new ArrayDeque<>();
	}
	
	public Context getActive() {
		return contexts.peek();
	}
	
	public Context push(JsonValue localContext, URL baseUrl) {
		return null;
	}
		
}
