package com.apicatalog.jsonld;

import java.io.Reader;
import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.web.media.MediaType;

/**
 * A JSON-LD document that can be processed by the processor.
 * 
 * Holds tree alike data structure as a content.
 * 
 * @param url        {@link URI} of the loaded document or <code>null</code> if
 *                   not available
 * @param content    {@link Map} or {@link Collection} representing materialized
 *                   document content
 * @param contenType The <a href=
 *                   "https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
 *                   of the loaded document, exclusive of any optional
 *                   parameters, or <code>null</code> if not available
 * @param profile    The value of any <code>profile</code> parameter retrieved
 *                   as part of the original {@link #contentType()} or
 *                   <code>null</code> if not available
 * @param context    {@link URI} referencing document context which is attached
 *                   to the HTTP Link header when profile attribute matches
 *                   <code>http://www.w3.org/ns/json-ld#context</code> or
 *                   <code>null</code> if not available
 *
 */
public record Document(
        URI url,
        Object content,
        MediaType contentType,
        String profile,
        URI context) {

    @FunctionalInterface
    public interface ContentReader {
        Object read(MediaType type, Reader is);
    }

    public Document {

        content = Objects.requireNonNull(content);

        if (!(content instanceof Map) && !(content instanceof Collection)) {
            throw new IllegalArgumentException(
                    "Unsupported content type: expected Map or Collection, but got " + content);
        }

        profile = profile == null && contentType != null
                ? contentType.findFirstParameter("profile").orElse(null)
                : profile;

        contentType = contentType != null
                ? new MediaType(contentType.type(), contentType.subtype())
                : null;
    }
}