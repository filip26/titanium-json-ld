package com.apicatalog.jsonld.input;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdInput;
import com.apicatalog.jsonld.api.JsonLdOptions;
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

		return new DocumentInput(
						options
							.getDocumentLoader()
							.loadDocument(
									documentUri,
									new LoadDocumentOptions().setExtractAllScripts(options.isExtractAllScripts()))
							)
					;
	}

}
