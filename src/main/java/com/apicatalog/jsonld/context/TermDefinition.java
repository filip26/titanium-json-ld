package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.Collection;

import javax.json.JsonValue;

import com.apicatalog.jsonld.grammar.DirectionType;

public class TermDefinition {

	// mandatory
	URI uriMapping;
	
	boolean prefixFlag;
	
	boolean protectedFlag;
	
	boolean reversePropertyFlag;
	
	// optional
	URI baseUrl;
	
	JsonValue context;
	
	Collection<String> containerMapping;
	
	DirectionType directionMapping;
	
	String indexMapping;
	
	String languageMapping;
	
	String nestValue;
	
	URI typeMapping; 
	
	public TermDefinition(boolean prefixFlag, boolean protectedFlag, boolean reversePropertyFlag) {
		this.prefixFlag = prefixFlag;
		this.protectedFlag = protectedFlag;
		this.reversePropertyFlag = reversePropertyFlag;
	}
	
	public void setContext(JsonValue context) {
		this.context = context;
	}
	
	public void setBaseUrl(URI baseUrl) {
		this.baseUrl = baseUrl;
	}
}
