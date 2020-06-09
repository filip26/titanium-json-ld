package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;
import com.apicatalog.jsonld.api.builder.FlatteningApi;
import com.apicatalog.jsonld.api.builder.FramingApi;
import com.apicatalog.jsonld.api.builder.FromRdfApi;
import com.apicatalog.jsonld.api.builder.ToRdfApi;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.RdfDataset;

public final class JsonLd {

    private JsonLd() {
    }
    
    public static final ExpansionApi expand(final String documentLocation) {

        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        return new ExpansionApi(URI.create(documentLocation));
    }

    public static final ExpansionApi expand(final URI documentUri) {
            
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        return new ExpansionApi(documentUri);
    }
    
    public static final CompactionApi compact(String documentLocation, String contextLocation) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        assertLocation(contextLocation, "contextLocation");
                
        return compact(URI.create(documentLocation), URI.create(contextLocation));
    }
    
    public static final CompactionApi compact(URI documentUri, URI contextUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertUri(contextUri, "contextUri");
        
        return new CompactionApi(documentUri, contextUri);
    }

    public static final CompactionApi compact(String documentLocation, JsonStructure context) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        if (context == null) {
            throw new IllegalArgumentException("A context is null.");
        }

        return new CompactionApi(URI.create(documentLocation), context);
    }

    public static final CompactionApi compact(URI documentUri, JsonStructure context) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        if (context == null) {
            throw new IllegalArgumentException("A context is null.");
        }
        
        return new CompactionApi(documentUri, context);
    }

    public static final FlatteningApi flatten(String documentLocation) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        return new FlatteningApi(URI.create(documentLocation));
    }
    
    public static final FlatteningApi flatten(URI documentUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        return new FlatteningApi(documentUri);
    }

    public static final FramingApi frame(URI documentUri, URI frameUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertUri(frameUri, "frameUri");
        
        return new FramingApi(documentUri, frameUri);
    }

    public static final ToRdfApi toRdf(String documentLocation) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        return new ToRdfApi(URI.create(documentLocation));
    }
    
    public static final ToRdfApi toRdf(URI documentUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        return new ToRdfApi(documentUri);
    }

    public static final FromRdfApi fromRdf(RdfDataset dataset) {
        
        if (dataset == null) {
            throw new IllegalArgumentException("A dataset is null.");
        }

        return new FromRdfApi(dataset);
    }
    
    public static final JsonLdProcessor createProcessor() {
        throw new UnsupportedOperationException();
    }
    
    private static final void assertLocation(final String location, final String param) {
        if (location == null || location.isBlank()) {
            throw new IllegalArgumentException("'" + param + "' is null or blank string.");
        }
        
        if (UriUtils.isNotAbsoluteURI(location)) {
            throw new IllegalArgumentException("'" + param + "' is not an absolute URI [" + location + "].");
        }
    }
    
    private static final void assertUri(final URI uri, final String param) {
        if (uri == null) {
            throw new IllegalArgumentException("'" + param + "' is null.");
        }

        if (!uri.isAbsolute()) {
            throw new IllegalArgumentException("'" + param + "' is not an absolute URI [" + uri + "].");
        }
    }
    
    private static final String DOCUMENT_LOCATION_PARAM_NAME = "documentLocation";
    private static final String DOCUMENT_URI_PARAM_NAME = "documentUri";
}
