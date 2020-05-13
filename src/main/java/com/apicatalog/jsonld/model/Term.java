package com.apicatalog.jsonld.model;

public final class Term implements PropertyName {

	private final String name;
	
	protected Term(final String name) {
		this.name = name;
	}
	
	public static final Term of(final String name) {
		if (name == null) {
			throw new IllegalArgumentException("A term must not be null");
		}
		if (name.isBlank()) {
			throw new IllegalArgumentException("A term must not be an empty string");
		}
		if (name.startsWith("@")) {
			throw new IllegalArgumentException("A term should not start with '@' character");
		}
		if (name.contains(":")) {
			throw new IllegalArgumentException("A term should not include a colon (:)");
		}
		
		return new Term(name);
	}
	
	@Override
	public String getName() {
		return name;
	}
	
}
