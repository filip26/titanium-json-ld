package com.apicatalog.jsonld.model;

public final class Keyword implements PropertyName {

	public static final Keyword BASE = new Keyword("@base");
	
	public static final Keyword CONTAINER = new Keyword("@container");
	
	public static final Keyword CONTEXT = new Keyword("@context");
	
	public static final Keyword DIRECTION = new Keyword("@direction");
	
	public static final Keyword GRAPH = new Keyword("@graph");
	
	public static final Keyword ID = new Keyword("@id");
	
	public static final Keyword IMPORT = new Keyword("@import");
	
	public static final Keyword INCLUDED = new Keyword("@included");
	
	public static final Keyword INDEX = new Keyword("@index");
	
	public static final Keyword JSON = new Keyword("@json");
	
	public static final Keyword LANGUAGE = new Keyword("@language");
	
	public static final Keyword LIST = new Keyword("@list");
	
	public static final Keyword NEST = new Keyword("@nest");
	
	public static final Keyword NONE = new Keyword("@none");
	
	public static final Keyword PREFIX = new Keyword("@prefix");
	
	public static final Keyword PROPAGATE = new Keyword("@propagate");
	
	public static final Keyword PROTECTED = new Keyword("@protected");
	
	public static final Keyword REVERSE = new Keyword("@reverse");
	
	public static final Keyword SET = new Keyword("@set");
	
	public static final Keyword TYPE = new Keyword("@type");
	
	public static final Keyword VALUE = new Keyword("@value");
	
	public static final Keyword VERSION = new Keyword("@version");
	
	public static final Keyword VOCAB = new Keyword("@vocab");
	
	private final String name;
	
	protected Keyword(final String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
}
