package com.apicatalog.jsonld.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.LoadDocumentCallback;
import com.apicatalog.jsonld.document.LoadDocumentOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.document.RemoteDocumentImpl;

public class DefaultDocumentLoader implements LoadDocumentCallback {

	@Override
	public RemoteDocument loadDocument(final URL url, final LoadDocumentOptions options) throws JsonLdError {
		
		try {
			
			URLConnection urlConnection = url.openConnection();
			
			urlConnection.setDoInput(false);
			urlConnection.setDoOutput(false);
			
			urlConnection.connect();
			
			//TODO String contentType = urlConnection.getContentType();
						
			String text = new BufferedReader(new InputStreamReader(urlConnection.getInputStream()))
					  .lines().collect(Collectors.joining("\n"));
			
			RemoteDocumentImpl remoteDocument = new RemoteDocumentImpl();
			
			JsonDocument document = new JsonDocument(text);
			
			remoteDocument.setDocument(document);
			
			return remoteDocument;
	
		} catch (IOException e) {
			throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
		}		
	}

}
