package com.apicatalog.jsonld.document;

import java.net.URI;

import com.apicatalog.jsonld.loader.LoadDocumentCallback;

/**
 * The {@link RemoteDocument} is used by a {@link LoadDocumentCallback} to
 * return information about a remote document or context.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#remotedocument">RemoteDocument
 *      Specification</a>
 *
 */
public class RemoteDocument {

	private String contextType;
	private URI contextUrl;

	private Document document;

	private URI documentUrl;
	private String profile;

	/**
	 * The <a href="https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
	 * of the loaded document, exclusive of any optional parameters.
	 * 
	 * @return
	 */
	public String getContextType() {
		return contextType;
	}

	/**
	 * 
	 * @return
	 */
	public URI getContextUrl() {
		return contextUrl;
	}

	/**
	 * The final URL of the loaded document. This is important to handle HTTP
	 * redirects properly.
	 * 
	 * @return
	 */
	public URI getDocumentUrl() {
		return documentUrl;
	}
	
	public void setDocumentUrl(URI documentUrl) {
		this.documentUrl = documentUrl;
	}

	/**
	 * The value of any <code>profile</code> parameter retrieved as part of the
	 * original {@link #getContextType()}.
	 * 
	 * @return
	 */
	public String getProfile() {
		return profile;
	}

	/**
	 * The retrieved document. This can either be the raw payload or the already
	 * parsed document.
	 * 
	 * @return
	 */
	public Document getDocument() {
		return document;
	}

	public void setDocument(Document document) {
		this.document = document;
	}	
}
