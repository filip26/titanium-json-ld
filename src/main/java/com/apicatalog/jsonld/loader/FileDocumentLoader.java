package com.apicatalog.jsonld.loader;

import java.io.FileReader;
import java.io.IOException;
import java.net.URL;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.DocumentReader;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.document.RemoteDocumentImpl;

public class FileDocumentLoader implements LoadDocumentCallback {

	@Override
	public RemoteDocument loadDocument(final URL url, final LoadDocumentOptions options) throws JsonLdError {

		if (!"file".equalsIgnoreCase(url.getProtocol())) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}

		try {
			DocumentReader document = new DocumentReader(new FileReader(url.getFile()));

			RemoteDocumentImpl remoteDocument = new RemoteDocumentImpl();
			remoteDocument.setDocument(document);
			remoteDocument.setDocumentUrl(url);

			return remoteDocument;

		} catch (IOException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
		}
	}

}
