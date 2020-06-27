package com.apicatalog.jsonld.document;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.Optional;

import javax.json.JsonException;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.io.nquad.NQuadsReaderException;

public class RdfDocument implements Document {

    private final MediaType contentType;
    private final RdfDataset dataset;
    private final String profile;

    private URI documentUrl;
    private URI contentUrl;
    
    private RdfDocument(final MediaType type, final String profile, final RdfDataset dataset) {
        this.contentType = type;
        this.profile = profile;
        this.dataset = dataset;
    }

    public static final Document of(final RdfDataset dataset) {
        
        if (dataset == null) {
            throw new IllegalArgumentException("RDF dataset cannot be a null.");
        }
        
        return new RdfDocument(MediaType.N_QUADS, null, dataset);
    }
    
    public static final Document of(final MediaType type, final InputStream is)  throws JsonLdError {
        
        assertContentType(type);
        
        try {

            RdfDataset dataset  = Rdf.createReader(is, RdfFormat.N_QUADS).readDataset();

            return new RdfDocument(type, null, dataset);
            
        } catch (JsonException | IOException | NQuadsReaderException | UnsupportedFormatException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    public static final Document of(final MediaType type, final Reader reader)  throws JsonLdError {
        
        assertContentType(type);
        
        try {

            RdfDataset dataset  = Rdf.createReader(reader, RdfFormat.N_QUADS).readDataset();

            return new RdfDocument(type, null, dataset);
            
        } catch (JsonException | IOException | NQuadsReaderException | UnsupportedFormatException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    public static final boolean accepts(final MediaType contentType) {
        return contentType != null && MediaType.N_QUADS.match(contentType);        
    }
    
    private static final void assertContentType(final MediaType contentType) {
        if (!accepts(contentType)) {
            throw new IllegalArgumentException(
                    "Unsupported media type '" + contentType 
                    + "'. Supported content types are [" 
                    + MediaType.N_QUADS 
                    + "]");
        }
    }
    
    @Override
    public MediaType getContentType() {
        return contentType;
    }

    @Override
    public URI getContextUrl() {
        return contentUrl;
    }

    @Override
    public void setContextUrl(URI contextUrl) {
        this.contentUrl = contextUrl;
    }

    @Override
    public URI getDocumentUrl() {
        return documentUrl;
    }

    @Override
    public void setDocumentUrl(URI documentUrl) {
        this.documentUrl = documentUrl;
    }

    @Override
    public Optional<String> getProfile() {
        return Optional.ofNullable(profile);
    }

    @Override
    public Optional<JsonStructure> getJsonContent() throws JsonLdError {
        return Optional.empty();
    }

    @Override
    public Optional<RdfDataset> getRdfContent() throws JsonLdError {
        return Optional.of(dataset);
    }
}
