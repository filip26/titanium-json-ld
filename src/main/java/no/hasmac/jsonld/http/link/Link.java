/*
 * Copyright 2020 the original author or authors.
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
package no.hasmac.jsonld.http.link;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import no.hasmac.jsonld.http.media.MediaType;

/**
 *
 * @see <a href="https://tools.ietf.org/html/rfc8288">Web Linking</a>
 *
 */
public final class Link {

    private final URI contextUri;

    private final URI targetUri;

    private final Set<String> relations;

    private final MediaType type;

    private final LinkAttributes attributes;

    protected Link(URI contextUri, URI targetUri, Set<String> relations, final MediaType type, final LinkAttributes attributes) {
        this.contextUri = contextUri;
        this.targetUri = targetUri;
        this.relations = relations;
        this.type = type;
        this.attributes = attributes;
    }

    public static Collection<Link> of(final String linkHeader) {
        return of(linkHeader, null);
    }

    public static Collection<Link> of(final String linkHeader, final URI baseUri) {
        if (linkHeader == null) {
            throw new IllegalArgumentException("Link header value cannot be null.");
        }

        return new LinkHeaderParser(baseUri).parse(linkHeader);
    }

    public URI target() {
        return targetUri;
    }

    public Optional<URI> context() {
        return Optional.ofNullable(contextUri);
    }

    public Set<String> relations() {
        return Collections.unmodifiableSet(relations);
    }

    public Optional<MediaType> type() {
        return Optional.ofNullable(type);
    }

    public LinkAttributes attributes() {
        return attributes;
    }
}
