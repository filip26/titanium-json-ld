package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.input.JsonStructureInput;
import com.apicatalog.jsonld.input.RemoteLocation;

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
public interface JsonLdInput {

    public static JsonLdInput of(JsonStructure jsonStructure) {
        return new JsonStructureInput(jsonStructure);
    }

    public static JsonLdInput of(URI documentUri) {
        return new RemoteLocation(documentUri);
    }

    public static JsonLdInput of(RemoteDocument remoteDocument) {
        return null;
    }

}
