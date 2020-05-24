package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.document.RemoteDocumentImpl;

public class JavaResourceLoader implements LoadDocumentCallback {

	@Override
	public RemoteDocument loadDocument(URI url, LoadDocumentOptions options) throws JsonLdError {
		
		
		if (!"classpath".equals(url.getScheme())) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
		}

		try (InputStream is = getClass().getResourceAsStream(url.toString().substring("classpath:".length()))) {

			Document document = JsonDocument.parse(new InputStreamReader(is));
			RemoteDocumentImpl remoteDocument = new RemoteDocumentImpl();
			remoteDocument.setDocument(document);
			remoteDocument.setDocumentUrl(url);	//TODO set final URL 

			return remoteDocument;
			
			
		} catch (IOException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
		}
	}

}
