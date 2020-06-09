package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;
import com.apicatalog.jsonld.api.builder.FlatteningApi;
import com.apicatalog.jsonld.api.builder.FromRdfApi;
import com.apicatalog.jsonld.api.builder.ToRdfApi;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.RdfDataset;

public final class JsonLd {

    private JsonLd() {
    }
    
    public static final ExpansionApi expand(final String documentLocation) {

        assertLocation(documentLocation, "documentLocation");
        
        return new ExpansionApi(URI.create(documentLocation));
    }

    public static final ExpansionApi expand(final URI documentUri) {
            
        assertUri(documentUri, "documentUri");
        
        return new ExpansionApi(documentUri);
    }
    
    public static final CompactionApi compact(String documentLocation, String contextLocation) {
        
        assertLocation(documentLocation, "documentLocation");
        assertLocation(contextLocation, "contextLocation");
                
        return compact(URI.create(documentLocation), URI.create(contextLocation));
    }
    
    public static final CompactionApi compact(URI documentUri, URI contextUri) {
        
        assertUri(documentUri, "documentUri");
        assertUri(contextUri, "contextUri");
        
        return new CompactionApi(documentUri, contextUri);
    }

    public static final CompactionApi compact(String documentLocation, JsonStructure context) {
        
        assertLocation(documentLocation, "documentLocation");
        
        if (context == null) {
            throw new IllegalArgumentException("A context is null.");
        }

        return new CompactionApi(URI.create(documentLocation), context);
    }

    public static final CompactionApi compact(URI documentUri, JsonStructure context) {
        
        assertUri(documentUri, "documentUri");
        
        if (context == null) {
            throw new IllegalArgumentException("A context is null.");
        }
        
        return new CompactionApi(documentUri, context);
    }

    public static final FlatteningApi flatten(String documentLocation) {
        
        assertLocation(documentLocation, "documentLocation");
        
        return new FlatteningApi(URI.create(documentLocation));
    }
    
    public static final FlatteningApi flatten(URI documentUri) {
        
        assertUri(documentUri, "documentUri");
        
        return new FlatteningApi(documentUri);
    }

    public static final ToRdfApi toRdf(String documentLocation) {
        
        assertLocation(documentLocation, "documentLocation");
        
        return new ToRdfApi(URI.create(documentLocation));
    }
    
    public static final ToRdfApi toRdf(URI documentUri) {
        
        assertUri(documentUri, "documentUri");
        
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
}
