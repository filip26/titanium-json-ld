package com.apicatalog.jsonld.input;

import com.apicatalog.jsonld.JsonLdInput;
import com.apicatalog.jsonld.document.RemoteDocument;

public class DocumentInput implements JsonLdInput {

	private final RemoteDocument remoteDocument;
	
	public DocumentInput(final RemoteDocument remoteDocument) {
		this.remoteDocument = remoteDocument;
	}

	@Override
	public Type getType() {
		return Type.DOCUMENT;
	}
	
}
