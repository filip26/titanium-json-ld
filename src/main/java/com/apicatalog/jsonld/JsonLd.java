package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;
import com.apicatalog.jsonld.api.builder.FlatteningApi;
import com.apicatalog.jsonld.api.builder.FramingApi;
import com.apicatalog.jsonld.api.builder.FromRdfApi;
import com.apicatalog.jsonld.api.builder.ToRdfApi;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.RdfDataset;

/**
 * {@link JsonLd} is the main interface you should use to process JSON-LD with Titanium.
 * 
 *  This class provides all methods to process JSON-LD.
 *
 */
public final class JsonLd {

    private static final String DOCUMENT_LOCATION_PARAM_NAME = "documentLocation";
    private static final String DOCUMENT_URI_PARAM_NAME = "documentUri";

    private JsonLd() {
    }
    
    /**
     * Expands the referenced document.
     * 
     * @param documentLocation {@code IRI} referencing JSON-LD document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters 
     */
    public static final ExpansionApi expand(final String documentLocation) {

        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        return new ExpansionApi(URI.create(documentLocation));
    }

    /**
     * Expands the referenced document.
     * 
     * @param documentUri {@link URI} referencing JSON-LD document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters 
     */
    public static final ExpansionApi expand(final URI documentUri) {
            
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        return new ExpansionApi(documentUri);
    }

    /**
     * Compacts the referenced document using the context.
     * 
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param contextLocation {@code IRI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters 
     */
    public static final CompactionApi compact(String documentLocation, String contextLocation) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        assertLocation(contextLocation, "contextLocation");
                
        return compact(URI.create(documentLocation), URI.create(contextLocation));
    }

    /**
     * Compacts the referenced document using the context.
     * 
     * @param documentUri {@link URI} referencing JSON-LD document to compact
     * @param contextUri {@link URI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters 
     */
    public static final CompactionApi compact(URI documentUri, URI contextUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertUri(contextUri, "contextUri");
        
        return new CompactionApi(documentUri, contextUri);
    }

    /**
     * Compacts the referenced document using the context.
     * 
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param context {@link JsonObject} representing the context or {@link JsonArray} consisting of {@link JsonObject} and {@link JsonString} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters 
     */
    public static final CompactionApi compact(String documentLocation, JsonStructure context) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        if (context == null) {
            throw new IllegalArgumentException("A context is null.");
        }

        return new CompactionApi(URI.create(documentLocation), context);
    }

    /**
     * Compacts the referenced document using the context.
     * 
     * @param documentUri {@code URI} referencing JSON-LD document to compact
     * @param context {@link JsonObject} representing the context or {@link JsonArray} consisting of one or many {@link JsonObject} and {@link JsonString} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters 
     */
    public static final CompactionApi compact(URI documentUri, JsonStructure context) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        if (context == null) {
            throw new IllegalArgumentException("A context is null.");
        }
        
        return new CompactionApi(documentUri, context);
    }

    /**
     * Flattens the given input and optionally compacts it using context.
     * 
     * @param documentLocation {@code IRI} referencing JSON-LD document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static final FlatteningApi flatten(String documentLocation) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        return new FlatteningApi(URI.create(documentLocation));
    }

    /**
     * Flattens the given input and optionally compacts it using context.
     * 
     * @param documentUri {@code URI} referencing JSON-LD document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static final FlatteningApi flatten(URI documentUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        return new FlatteningApi(documentUri);
    }

    /**
     *  Frames the given input using frame.
     *  
     * @param documentUri {@code URI} referencing JSON-LD document to frame
     * @param frameUri {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(URI documentUri, URI frameUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertUri(frameUri, "frameUri");
        
        return new FramingApi(documentUri, frameUri);
    }

    /**
     * Transforms the given input into an {@link RdfDataset}.
     * 
     * @param documentLocation {@code IRI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final ToRdfApi toRdf(String documentLocation) {
        
        assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME);
        
        return new ToRdfApi(URI.create(documentLocation));
    }

    /**
     * Transforms the given input into an {@link RdfDataset}.
     * 
     * @param documentUri {@code URI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final ToRdfApi toRdf(URI documentUri) {
        
        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        
        return new ToRdfApi(documentUri);
    }

    /**
     * Transforms an {@link RdfDataset} into a JSON-LD document in expanded form.
     * 
     * @param dataset to transform
     * @return {@link FromRdfApi} allowing to set additional parameters
     */
    public static final FromRdfApi fromRdf(RdfDataset dataset) {
        
        if (dataset == null) {
            throw new IllegalArgumentException("A dataset is null.");
        }

        return new FromRdfApi(dataset);
    }
    
    private static final void assertLocation(final String location, final String param) {
        if (location == null || location.isBlank()) {
            throw new IllegalArgumentException("'" + param + "' is null or blank string.");
        }
        
        if (UriUtils.isNotAbsoluteUri(location)) {
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
