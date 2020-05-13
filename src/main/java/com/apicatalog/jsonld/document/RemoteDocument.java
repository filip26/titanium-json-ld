package com.apicatalog.jsonld.document;

import java.net.URL;

/**
 * The {@link RemoteDocument} is used by a {@link LoadDocumentCallback}
 * to return information about a remote document or context.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#remotedocument">RemoteDocument Specification</a>
 *
 */
public interface RemoteDocument {

	/**
	 * The <a href="https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
	 * of the loaded document, exclusive of any optional parameters.
	 * 
	 * @return
	 */
	String getContextType();
	
	/**
	 * 
	 * @return
	 */
	URL getContextUrl();
	
	/**
	 * The final URL of the loaded document. This is important to handle HTTP redirects properly.
	 * 
	 * @return
	 */
	URL getDocumentUrl();
	
	/**
	 * The value of any <code>profile</code> parameter retrieved as part of the original {@link #getContextType()}.
	 * 
	 * @return
	 */
	String getProfile();
	
	/**
	 * The retrieved document. This can either be the raw payload or the already parsed document.
	 * 
	 * @return
	 */
	Document getDocument();
}
