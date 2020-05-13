package com.apicatalog.jsonld.remote;

/**
 * The {@link LoadDocumentOptions} type is used to pass various options to the {@link LoadDocumentCallback}.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#loaddocumentoptions">LoadDocumentOptions Specification</a>
 *
 */
public class LoadDocumentOptions {

	private final boolean extractAllScripts;
	
	public LoadDocumentOptions(boolean extractAllScripts) {
		this.extractAllScripts = extractAllScripts;
	}
	
	public boolean isExtractAllScripts() {
		return extractAllScripts;
	}
	
}
