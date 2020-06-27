package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;

/**
 * The {@link LoadDocumentCallback} defines a callback that custom document
 * loaders have to implement to be used to retrieve remote documents and
 * contexts. The callback returns a Promise resolving to a RemoteDocument. On
 * failure, the Promise with a JsonLdError having an appropriate error code.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#loaddocumentcallback">LoadDocumentCallback
 *      Specification</a>
 *
 */
public interface LoadDocumentCallback {

    Document loadDocument(URI url, LoadDocumentOptions options) throws JsonLdError;

}
