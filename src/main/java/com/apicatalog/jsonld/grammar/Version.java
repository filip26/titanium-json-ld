package com.apicatalog.jsonld.grammar;

public enum Version {

	V1_0("json-ld-1.0"), V1_1("json-ld-1.1");

	private final String text;

	Version(final String text) {
		this.text = text;
	}

	public static Version of(String text) {

		if (text == null) {
			throw new IllegalArgumentException();
		}

		if (V1_0.text.equalsIgnoreCase(text)) {
			return V1_0;
		}
		if (V1_1.text.equalsIgnoreCase(text)) {
			return V1_1;
		}
		return null;
	}

	@Override
	public String toString() {
		return text;
	}
}
