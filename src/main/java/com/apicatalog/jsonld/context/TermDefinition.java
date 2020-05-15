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
	
	
}
