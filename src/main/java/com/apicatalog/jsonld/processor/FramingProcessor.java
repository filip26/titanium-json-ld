package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-framing/#dom-jsonldprocessor-frame">JsonLdProcessor.frame()</a>
 *
 */
public final class FramingProcessor {

    private FramingProcessor() {
    }
    
    public static final JsonObject frame(final URI input, final URI frame, final JsonLdOptions options) throws JsonLdError {
        
        return null;
    }
}
