package com.apicatalog.jsonld.impl;

import java.net.MalformedURLException;
import java.net.URI;
import java.util.Collection;

import com.apicatalog.json.JsonParser;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdInput;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdRecord;
import com.apicatalog.jsonld.document.LoadDocumentOptions;
import com.apicatalog.jsonld.document.RemoteDocument;

public class RemoteInput implements JsonLdInput {

	private final URI uri;
	
	public RemoteInput(URI uri) {
		this.uri = uri;
	}

	@Override
	public Collection<JsonLdRecord> getRecords(final JsonLdOptions options, final JsonParser jsonParser) throws JsonLdError {

		if (options == null) {
			throw new IllegalArgumentException();
		}
		if (options.getDocumentLoader().isEmpty()) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}
		
		try {
			
			final RemoteDocument remoteDocument = options.getDocumentLoader().get().loadDocument(uri.toURL(), new LoadDocumentOptions(options.isExtractAllScripts()));
			
			return remoteDocument.getDocument().parse(jsonParser);
			
		} catch (MalformedURLException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
		}
	}
	
}
