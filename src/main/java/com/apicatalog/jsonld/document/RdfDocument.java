package com.apicatalog.jsonld.document;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.json.JsonException;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.error.RdfReaderException;
import com.apicatalog.rdf.io.error.UnsupportedContentException;

public final class RdfDocument implements Document {

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

    /**
     * Create a new document from {@link RdfDataset}. Sets {@link MediaType#N_QUADS} as the content type.
     *
     * @param dataset representing parsed RDF content
     * @return {@link Document} representing RDF document
     */
    public static final Document of(final RdfDataset dataset) {
        return of(MediaType.N_QUADS, dataset);
    }

    /**
     * Create a new document from {@link RdfDataset}.
     *
     * @param contentType reflecting the provided {@link RdfDataset}, only {@link MediaType#N_QUADS} is supported
     * @param dataset representing parsed RDF content
     * @return {@link Document} representing RDF document
     */
    public static final Document of(final MediaType contentType, final RdfDataset dataset) {
        
        assertContentType(contentType);
        
        if (dataset == null) {
            throw new IllegalArgumentException("RDF dataset cannot be a null.");
        }
        
        return new RdfDocument(contentType, null, dataset);
    }

    /**
     * Create a new document from content provided by {@link InputStream}. Sets {@link MediaType#N_QUADS} as the content type.
     *
     * @param is representing parsed RDF content
     * @return {@link Document} representing RDF document
     */
    public static final Document of(final InputStream is)  throws JsonLdError {
        return of(MediaType.N_QUADS, is);
    }
    
    public static final Document of(final MediaType type, final InputStream is)  throws JsonLdError {
        
        assertContentType(type);
        
        try {

            RdfDataset dataset  = Rdf.createReader(type, is).readDataset();

            return new RdfDocument(type, null, dataset);
            
        } catch (JsonException | IOException | RdfReaderException | UnsupportedContentException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    /**
     * Create a new document from content provided by {@link Reader}. Sets {@link MediaType#N_QUADS} as the content type.
     *
     * @param reader providing RDF content
     * @return {@link Document} representing RDF document
     */
    public static final Document of(final Reader reader)  throws JsonLdError {
        return of(MediaType.N_QUADS, reader);
    }
    
    public static final Document of(final MediaType type, final Reader reader)  throws JsonLdError {
        
        assertContentType(type);
        
        try {

            RdfDataset dataset  = Rdf.createReader(type, reader).readDataset();

            return new RdfDocument(type, null, dataset);
            
        } catch (JsonException | IOException | RdfReaderException | UnsupportedContentException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    public static final boolean accepts(final MediaType contentType) {
        return Rdf.canRead().contains(contentType);
    }
    
    private static final void assertContentType(final MediaType contentType) {
        if (!accepts(contentType)) {
            throw new IllegalArgumentException(
                    "Unsupported media type '" + contentType 
                    + "'. Supported content types are [" 
                    + (Rdf.canRead().stream().map(MediaType::toString).collect(Collectors.joining(", ")))
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
    public Optional<RdfDataset> getRdfContent() {
        return Optional.of(dataset);
    }
}
