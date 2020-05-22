package com.apicatalog.jsonld.input;

import java.net.MalformedURLException;
import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdInput;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public class RemoteLocation implements JsonLdInput {

	private final URI documentUri;

	public RemoteLocation(URI documentUri) {
		this.documentUri = documentUri;
	}

	@Override
	public Type getType() {
		return Type.LOCATION;
	}

	public DocumentInput fetch(final JsonLdOptions options) throws JsonLdError {

		if (options == null) {
			throw new IllegalArgumentException();
		}
		if (options.getDocumentLoader() == null) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}

		try {

			return new DocumentInput(options.getDocumentLoader().loadDocument(documentUri.toURL(),
					new LoadDocumentOptions().setExtractAllScripts(options.isExtractAllScripts())));

		} catch (MalformedURLException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
		}
	}

}
