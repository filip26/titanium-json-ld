package com.apicatalog.jsonld.http;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc7231#section-3.1.1.1">Media Type</a>
 *
 */
public final class MediaType {

    private static final String TYPE_APPLICATION = "application";
    private static final String TYPE_TEXT = "text";    
    private static final String WILDCARD = "*";
    
    public static final MediaType HTML = new MediaType(TYPE_TEXT, "html"); 
    
    public static final MediaType JSON_LD = new MediaType(TYPE_APPLICATION, "ld+json");
    
    public static final MediaType JSON = new MediaType(TYPE_APPLICATION, "json");

    public static final MediaType XHTML = new MediaType(TYPE_APPLICATION, "xhtml+xml");
    
    public static final MediaType ANY = new MediaType(WILDCARD, WILDCARD);
    
    private final String type;
    private final String subtype;
    
    private final Map<String, String> parameters;

    protected MediaType(String type, String subtype, Map<String, String> parameters) {
        this.type = type;
        this.subtype = subtype;
        this.parameters = parameters;
        
    }

    public MediaType(String type, String subtype) {
        this(type, subtype, Collections.emptyMap());
    }

    public boolean match(MediaType mediaType) {
        return mediaType != null
                && (WILDCARD.equals(type) || WILDCARD.equals(mediaType.type) || Objects.equals(type, mediaType.type))
                && (WILDCARD.equals(subtype) || WILDCARD.equals(mediaType.subtype) || Objects.equals(subtype, mediaType.subtype))
                ;
    }

    public String type() {
        return subtype;
    }

    public String subtype() {
        return subtype;
    }
    
    public Set<String> paramNames() {
        return parameters.keySet();
    }
    
    public String paramValue(String name) {
        return parameters.get(name);
    }
    
    @Override
        public String toString() {
            return String.valueOf(type).concat("/").concat(subtype);
        }

    public static final MediaType valueOf(String contentTypeValue) {
        if (contentTypeValue == null || contentTypeValue.isBlank()) {
            return null;
        }
        return new MediaTypeParser(contentTypeValue).parse();
    }
}
