package com.apicatalog.jsonld.uri;

import java.net.URI;
import java.util.Objects;

public final class UriRelativizer {

    private UriRelativizer() {
    }

    public static final String relativize(final URI base, final String uri) {

        if (base == null) {
            return uri;
        }
        
        return relativize(base, URI.create(uri));
    }
    
    public static final String relativize(final URI base, final URI uri) {

        if (base == null || !base.isAbsolute() || !uri.isAbsolute()) {
            return uri.toString();
        }
        
        if (!Objects.equals(base.getScheme(), uri.getScheme())) {
            return uri.toString();
        }
        
        if (!Objects.equals(base.getAuthority(), uri.getAuthority())) {
            return UriUtils.recompose(null, uri.getAuthority(), uri.getPath() , uri.getQuery(), uri.getFragment());
        }
        
        final Path path = Path.of(uri.getPath()).relativize(base.getPath());
        
        if (path.isNotEmpty()) {
            return UriUtils.recompose(null, null, path.toString() , uri.getQuery(), uri.getFragment());
        }
        
        if (!Objects.equals(base.getQuery(), uri.getQuery())) {
            return UriUtils.recompose(null, null, null , uri.getQuery(), uri.getFragment());
        }
                
        if (!Objects.equals(base.getFragment(), uri.getFragment())) {
            return UriUtils.recompose(null, null, null , null, uri.getFragment());            
        }

        return "";
    }
}
