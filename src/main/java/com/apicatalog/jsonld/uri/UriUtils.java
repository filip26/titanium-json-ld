package com.apicatalog.jsonld.uri;

import java.net.URI;

import com.apicatalog.jsonld.lang.Keywords;

public final class UriUtils {

    private UriUtils() {
    }

    public static final boolean isURI(String value) {

        return value != null 
                    && !value.isBlank() 
                    && !Keywords.hasForm(value)
                    && create(value) != null;
    }

    
    public static final URI create(String uri) {

        if (uri.endsWith(":")) {
            uri += ".";
            
        } else if (uri.endsWith("[") || uri.endsWith("]")) {
            uri = uri.substring(0, uri.length() - 1);
        }

        try {

            return URI.create(uri);
            
        } catch (IllegalArgumentException e) {
            return null;
        }
    }
    
    /**
     * @see <a href="https://tools.ietf.org/html/rfc3986#section-2.2">URI - Reserved
     *      Characters </a>
     * @param uri
     * @return
     */
    public static final boolean endsWithGenDelim(final String uri) {
        return uri.endsWith(":") || uri.endsWith("/") || uri.endsWith("?") || uri.endsWith("#") || uri.endsWith("[")
                || uri.endsWith("]") || uri.endsWith("@");
    }

    public static final boolean isNotURI(final String expandedTypeString) {
        return !isURI(expandedTypeString);
    }

    public static final boolean isNotAbsoluteURI(final String uri) {
        try {
            return !URI.create(uri).isAbsolute();
        } catch (IllegalArgumentException e) {
            return true;
        }
    }

    public static final boolean isAbsoluteUri(final String uri) {
        try {
            return URI.create(uri).isAbsolute();
        } catch (IllegalArgumentException e) {
            return false;
        }
    }
    
    static final String recompose(final String scheme, final String authority, final String path, final String query, final String fragment) {

        StringBuilder builder = new StringBuilder();

        if (isDefined(scheme)) {
            builder.append(scheme);
            builder.append(":");
        }
        if (authority != null) {
            builder.append("//");
            builder.append(authority);
        }
        if (isDefined(path)) {
            builder.append(path);
        }
        if (isDefined(query)) {
            builder.append('?');
            builder.append(query);
        }
        if (isDefined(fragment)) {
            builder.append('#');
            builder.append(fragment);
        }
        return builder.toString();
    }
    
    static final boolean isDefined(final String value) {
        return value != null && !value.isBlank();
    }

    static final boolean isNotDefined(final String value) {
        return value == null || value.isBlank();
    }

}
