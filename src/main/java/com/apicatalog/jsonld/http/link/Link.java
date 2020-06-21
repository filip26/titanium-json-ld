package com.apicatalog.jsonld.http.link;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.jsonld.http.media.MediaType;

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

    public static final Collection<Link> of(final String linkHeader) {
        return of(linkHeader, null);
    }
    
    public static final Collection<Link> of(final String linkHeader, final URI baseUri) {
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
