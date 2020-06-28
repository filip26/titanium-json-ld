package com.apicatalog.jsonld.api.impl;

import java.net.URI;

import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.document.Document;

public interface ContextApi<R> {
    
    /**
     * A context that is used to initialize the active context.
     * 
     * @param contextUri {@link URI} referring to a context
     * @return builder instance 
     */
    R context(URI contextUri);

    /**
     * A context that is used to initialize the active context.
     * 
     * @param contextLocation <code>IRI</code> referring to a context
     * @return builder instance 
     */
    R context(String contextLocation);

    /**
     * A context that is used to initialize the active context.
     * 
     * @param context {@link JsonObject}, a sequence of {@link JsonObject}, or a {@link JsonString} representing an <code>IRI</code> 
     * @return builder instance 
     */
    R context(JsonStructure context);

    /**
     * A context that is used to initialize the active context.
     * 
     * @param context {@link Document} representing a context 
     * @return builder instance 
     */
    R context(Document context);
}
