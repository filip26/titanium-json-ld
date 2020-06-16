package com.apicatalog.jsonld.loader;

import java.util.Objects;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc7231#section-3.1.1.1">Media Type</a>
 *
 */
public final class MediaType {

    private static final String TYPE_APPLICATION = "application";
    private static final String TYPE_TEXT = "application";    
    private static final String WILDCARD = "*";
    
    public static final MediaType HTML = new MediaType(TYPE_TEXT, "html"); 
    
    public static final MediaType JSON_LD = new MediaType(TYPE_APPLICATION, "ld+json");
    
    public static final MediaType JSON = new MediaType(TYPE_APPLICATION, "json");

    public static final MediaType XHTML = new MediaType(TYPE_APPLICATION, "xhtml+xml");
    
    public static final MediaType ANY = new MediaType(WILDCARD, WILDCARD);
    

    private final String type;
    private final String subtype;
    
    public MediaType(String type, String subtype) {
        this.type = type;
        this.subtype = subtype;
    }

    public boolean match(MediaType mediaType) {
        return mediaType != null
                && (WILDCARD.equals(type) || Objects.equals(type, mediaType.type))
                && (WILDCARD.equals(subtype) || Objects.equals(subtype, mediaType.subtype))
                ;
    }

    public String type() {
        return subtype;
    }

    public String subtype() {
        return subtype;
    }
    
    @Override
        public String toString() {
            return String.valueOf(type).concat("/").concat(subtype);
        }

    public static final MediaType valueOf(String contentTypeValue) {

        if (contentTypeValue == null || contentTypeValue.isBlank()) {
            return null;
        }
        
        int slash = contentTypeValue.indexOf('/');
        
        if (slash == -1) {
            return null;
        }
        
        int semicolon = contentTypeValue.indexOf(';', slash);
        
        if (semicolon != -1) {
            return new MediaType(contentTypeValue.substring(0, slash).strip(), contentTypeValue.substring(slash + 1, semicolon).strip());            
        }

        return new MediaType(contentTypeValue.substring(0, slash).strip(), contentTypeValue.substring(slash + 1).strip());
    }
}
