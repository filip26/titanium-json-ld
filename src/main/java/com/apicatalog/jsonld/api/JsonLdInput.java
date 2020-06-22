package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Arrays;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;

/**
 * The {@link JsonLdInput} interface is used to refer to an input value that
 * that may be a {@link JsonStructure}, {@link URI}, which can be dereferenced
 * to retrieve a valid JSON document, or an already dereferenced
 * {@link RemoteDocument}.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#webidl-295859679">JsonLdInput
 *      IDL</a>
 *
 */
public final class JsonLdInput {
    
    private final RemoteDocument document;
    
    public JsonLdInput(final RemoteDocument document) {
        this.document = document;
    }
    
    public static JsonLdInput of(final JsonObject...objects) {
        RemoteDocument document = new RemoteDocument();        
        document.setDocument(Document.of(Json.createArrayBuilder(Arrays.asList(objects)).build()));
        return new JsonLdInput(document);
    }

    public static JsonLdInput of(final URI documentUrl) {
        RemoteDocument document = new RemoteDocument();
        document.setDocumentUrl(documentUrl);
        return new JsonLdInput(document);
    }

    public static JsonLdInput of(final RemoteDocument remoteDocument) {
        return new JsonLdInput(remoteDocument);
    }

    public RemoteDocument asDocument() {
        return document;
    }
    
}
