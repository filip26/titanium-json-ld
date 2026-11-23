/*
 * Copyright 2020-2026 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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