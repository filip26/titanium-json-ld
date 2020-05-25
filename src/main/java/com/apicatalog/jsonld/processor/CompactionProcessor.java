package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class CompactionProcessor {

    CompactionProcessor() {
    }

    public static final JsonObject compact(URI input, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        return JsonObject.EMPTY_JSON_OBJECT;
    }

    public static final JsonObject compact(RemoteDocument input, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        return JsonObject.EMPTY_JSON_OBJECT;
    }
}
