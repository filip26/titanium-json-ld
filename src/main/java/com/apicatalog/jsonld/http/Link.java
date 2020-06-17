package com.apicatalog.jsonld.http;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc8288">Web Linking</a>
 *
 */
public final class Link {

    private final URI uri;
    private final Set<String> rel;
    private final Map<String, String> parameters;
    
    protected Link(URI uri, Set<String> rel, Map<String, String> parameters) {
        this.uri = uri;
        this.rel = rel;
        this.parameters = parameters; 
    }
    
    /**
     * 
     * @param linkHeader
     * @return
     * @see <a href=""></a>
     */
    public static final Collection<Link> valueOf(String linkHeader, URI baseUri) {
        return new LinkHeaderParser(linkHeader).parse(baseUri);
    }
    
    public URI uri() {
        return uri;
    }

    public Set<String> rel() {
        return rel;
    }

    public String type() {
        return parameters.get("type");
    }
    
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append('<');
        builder.append(uri);
        builder.append('>');
        
        if (parameters != null && !parameters.isEmpty()) {
            for (Map.Entry<String, String> parameter : parameters.entrySet()) {
                builder.append(';');
                builder.append(parameter.getKey());
                if (parameter.getValue() != null) {
                    builder.append('=');
                    builder.append(parameter.getValue());                    
                }
            }
        }

        return builder.toString();
    }
}
