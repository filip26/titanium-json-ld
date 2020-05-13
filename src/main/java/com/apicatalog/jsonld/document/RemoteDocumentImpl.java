package com.apicatalog.jsonld.document;

import java.net.URL;

public class RemoteDocumentImpl implements RemoteDocument {

	private String contextType;
	private URL contextUrl;
	
	private Document document;
	
	private URL documentUrl;
	private String profile;

	@Override
	public String getContextType() {
		return contextType;
	}

	@Override
	public URL getContextUrl() {
		return contextUrl;
	}

	@Override
	public URL getDocumentUrl() {
		return documentUrl;
	}

	@Override
	public String getProfile() {
		return profile;
	}

	@Override
	public Document getDocument() {
		return document;
	}
	
	public void setDocument(Document document) {
		this.document = document;
	}
	
	public void setDocumentUrl(URL documentUrl) {
		this.documentUrl = documentUrl;
	}
	
}
