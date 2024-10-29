package com.apicatalog.jsonld.loader;

import java.io.InputStream;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.Rdf;

class JsonDocumentResolver implements DocumentReaderResolver {

    private static final Logger LOGGER = Logger.getLogger(JsonDocumentResolver.class.getName());

    private MediaType fallbackContentType;

    public JsonDocumentResolver() {
        this.fallbackContentType = null;
    }

    /**
     * Return a reader or throw {@link JsonLdError} if there is no reader nor fallbackContentType.
     *
     * @param contentType content type of the requested reader
     * @return a reader allowing to transform an input into {@link Document}
     * @throws JsonLdError
     */
    public DocumentReader<InputStream> getReader(MediaType contentType) throws JsonLdError {
        return findReader(contentType)
                .or(() -> {

                    if (fallbackContentType != null) {
                        LOGGER.log(Level.WARNING, "Content type [{0}] is not acceptable, trying again with [{1}].", new Object[] { contentType, fallbackContentType});
                        return findReader(fallbackContentType);
                    }

                    return Optional.empty();
                })
                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED,
                    "Unsupported media type '" + contentType
                    + "'. Supported content types are ["
                    + MediaType.JSON_LD + ", "
                    + MediaType.JSON  + ", +json, "
                    + (Rdf.canRead().stream().map(MediaType::toString).collect(Collectors.joining(", ")))
                    + "]"
                    ));
    }

    public void setFallbackContentType(MediaType fallbackContentType) {
        this.fallbackContentType = fallbackContentType;
    }

    private static final Optional<DocumentReader<InputStream>> findReader(final MediaType type) {

        if (type == null) {
            return Optional.empty();
        }

        if (JsonDocument.accepts(type)) {
            return Optional.of((uri, context, is) -> JsonDocument.of(uri, context, type, is));
        }

        if (RdfDocument.accepts(type)) {
            return Optional.of((uri, context, is) -> RdfDocument.of(uri, type, is));
        }

        return Optional.empty();
    }
}
