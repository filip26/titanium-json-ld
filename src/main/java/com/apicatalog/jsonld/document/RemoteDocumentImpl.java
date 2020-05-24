package com.apicatalog.jsonld.document;

import java.net.URI;

@Deprecated
public class RemoteDocumentImpl implements RemoteDocument {

	private String contextType;
	private URI contextUrl;

	private Document document;

	private URI documentUrl;
	private String profile;

	@Override
	public String getContextType() {
		return contextType;
	}

	@Override
	public URI getContextUrl() {
		return contextUrl;
	}

	@Override
	public URI getDocumentUrl() {
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

	public void setDocumentUrl(URI documentUrl) {
		this.documentUrl = documentUrl;
	}

}
