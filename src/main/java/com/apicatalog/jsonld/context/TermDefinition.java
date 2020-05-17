package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import javax.json.JsonValue;

import com.apicatalog.jsonld.grammar.DirectionType;

public class TermDefinition {

	// mandatory
	String uriMapping;
	
	boolean prefixFlag;
	
	boolean protectedFlag;
	
	boolean reversePropertyFlag;
	
	// optional
	URI baseUrl;
	
	JsonValue localContext;
	
	Collection<String> containerMapping;
	
	DirectionType directionMapping;
	
	String indexMapping;
	
	String languageMapping;
	
	String nestValue;
	
	String typeMapping; 
	
	public TermDefinition(boolean prefixFlag, boolean protectedFlag, boolean reversePropertyFlag) {
		this.prefixFlag = prefixFlag;
		this.protectedFlag = protectedFlag;
		this.reversePropertyFlag = reversePropertyFlag;
		this.containerMapping = new ArrayList<>();
	}
	
	public void setLocalContext(JsonValue context) {
		this.localContext = context;
	}
	
	public void setBaseUrl(URI baseUrl) {
		this.baseUrl = baseUrl;
	}
	
	public String getUriMapping() {
		return uriMapping;
	}

	public void setUriMapping(String uriMapping) {
		this.uriMapping = uriMapping;
	}
		
	public String getLanguageMapping() {
		return languageMapping;
	}
	
	public DirectionType getDirectionMapping() {
		return directionMapping;
	}
	
	public String getTypeMapping() {
		return typeMapping;
	}
	
	public boolean isPrefixFlag() {
		return prefixFlag;
	}
	
	public void setLanguageMapping(String languageMapping) {
		this.languageMapping = languageMapping;
	}
	
	public void addContainerMapping(String mapping) {
		this.containerMapping.add(mapping);
	}
	
	public Collection<String> getContainerMapping() {
		return containerMapping;
	}
	
	public JsonValue getLocalContext() {
		return localContext;
	}
	
	public URI getBaseUrl() {
		return baseUrl;
	}
}
