package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.DocumentReader;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.document.RemoteDocumentImpl;

public class UrlConnectionLoader implements LoadDocumentCallback {

	@Override
	public RemoteDocument loadDocument(final URL url, final LoadDocumentOptions options) throws JsonLdError {

		try {
			URLConnection connection = url.openConnection();

			//TODO set accept header
			//TODO set timeout
			//TODO set follow redirects
			
			connection.setDoInput(false);
			connection.setDoInput(true);
			
			connection.connect();
			
			//TODO check response headers
			
			DocumentReader document = new DocumentReader(new InputStreamReader(connection.getInputStream()));
			RemoteDocumentImpl remoteDocument = new RemoteDocumentImpl();
			remoteDocument.setDocument(document);
			remoteDocument.setDocumentUrl(url);	//TODO set final URL 

			return remoteDocument;
			
		} catch (IOException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);			
		}		
	}

}
