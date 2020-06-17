package com.apicatalog.jsonld.http;

import java.net.URI;
import java.util.Collection;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc8288">Web Linking</a>
 *
 */
public final class Link {

    
    private Link() {
        
    }
    
    /**
     * 
     * @param linkHeader
     * @return
     * @see <a href=""></a>
     */
    public static final Collection<Link> parseHttpHeader(String linkHeader) {
        
        return null;
    }

    public URI uri() {
        return null;
    }

    public String rel() {
        // TODO Auto-generated method stub
        return null;
    }

    public String type() {
        // TODO Auto-generated method stub
        return null;
    }
    
}
