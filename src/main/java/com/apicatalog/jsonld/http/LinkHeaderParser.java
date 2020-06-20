package com.apicatalog.jsonld.http;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.uri.UriResolver;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc8288#appendix-B">Appendix B.  Algorithms for Parsing Link Header Fields</a>
 *
 */
final class LinkHeaderParser {

    private enum State { INIT, URI_REF, PARAMS, PARAM_NAME, PARAM_VALUE, STRING_VALUE, LITERAL_VALUE, ESCAPE, UNEXPECTED }
    
    private final char[] linkHeader;

    // runtime
    private Set<Link> links;
    
    private State state;
    private int lastIndex;

    private String targetUri;
    private String paramName;
    private StringBuilder stringValue;
    
    private Map<String, String> params;
    
    public LinkHeaderParser(String linkHeader) {
        this.linkHeader = linkHeader.toCharArray();
    }

    public Set<Link> parse(URI baseUri) {
        
        resetState();
        
        for (int i=0; i < linkHeader.length; i++) {
    
            final char ch = linkHeader[i];
            
            switch (state) {
            case INIT:
                initParser(ch, i);
                break;
                
            case URI_REF:
                parseUri(ch, i, baseUri);
                break;
                
            case PARAMS:
                parseParameters(ch, i);
                break;
                
            case PARAM_NAME:
                parseParamName(ch, i);
                break;
                
            case PARAM_VALUE:
                parseParamValue(ch, i);
                break;
            
            case LITERAL_VALUE:
                parseLiteral(ch, i);
                break;                

            case STRING_VALUE:
                parseString(ch);
                break;
            
            case ESCAPE:
                escape(ch);
                break;
                
            default:
                return links;
            }            
        }
        
        return doLastState();
    }
    
    private static final Link create(final String uri, final Map<String, String> params) {
        
        Set<String> rel = Collections.emptySet();
        
        if (params.containsKey("rel") && params.get("rel") != null) {
            rel = new HashSet<>(Arrays.asList(params.get("rel").split("(\\s\\t)+")));
        }

        return new Link(URI.create(uri), rel, params);
    }
    
    private final void resetState() {
        links = new LinkedHashSet<>();
        
        state = State.INIT;

        targetUri = null;
        paramName = null;
        stringValue = new StringBuilder();
        
        params = new HashMap<>();
        
        lastIndex = -1;
    }
    
    private final Set<Link> doLastState() {
        switch (state) {
        case PARAM_NAME:
            if (lastIndex < linkHeader.length) {
                paramName = String.valueOf(linkHeader, lastIndex,  linkHeader.length - lastIndex).strip().toLowerCase();
            }
            break;
            
        case LITERAL_VALUE:
            if (lastIndex < linkHeader.length) {
                params.put(paramName, String.valueOf(linkHeader, lastIndex,  linkHeader.length - lastIndex).strip());
                paramName = null;
            }
            break;

        case UNEXPECTED:
            return links;
            
        default:
            break;
        }
        return addLastLink();
    }
    
    private final Set<Link> addLastLink() {
        if (paramName != null) {
            if (stringValue.length() > 0) {
                params.put(paramName, null);
            } else {
                params.put(paramName, stringValue.toString());   
            }
        }
        
        if (targetUri != null) {
            links.add(create(targetUri, params));
        }
    
        return links;
    }
    
    private final void parseString(final char ch) {
        if (ch == '"') {
            params.put(paramName, stringValue.toString());
            stringValue.setLength(0);
            paramName = null;
            state = State.PARAMS;
            return;
        }
        if (ch == '\\') {
            state = State.ESCAPE;
            return;
        }
        stringValue.append(ch);
    }
    
    private final void parseLiteral(final char ch, final int iterator) {
        
        if (ch == ';') {
            params.put(paramName, String.valueOf(linkHeader, lastIndex,  iterator - lastIndex).strip());
            lastIndex = iterator + 1;
            paramName = null;
            state = State.PARAM_NAME;
                    
        } else if (ch == ',') {
            params.put(paramName, String.valueOf(linkHeader, lastIndex,  iterator - lastIndex).strip());
            links.add(create(targetUri, params));
            paramName = null;
            targetUri = null;
            params = new HashMap<>();
            state = State.INIT;
        }
    }
    
    private final void parseParamValue(final char ch, final int iterator) {
        if (Character.isSpaceChar(ch) || ch == '\t') {
            return;
        }
        if (ch == '"') {
            lastIndex = iterator + 1;
            state = State.STRING_VALUE;
            return;
        }
        lastIndex = iterator;
        state = State.LITERAL_VALUE;
    }
    
    private final void parseParamName(final char ch, final int iterator) {
        if (ch == '=') {
            paramName = String.valueOf(linkHeader, lastIndex,  iterator - lastIndex).strip().toLowerCase();
            state = State.PARAM_VALUE;
            return;
        }
        if (ch == ';') {
            params.put(String.valueOf(linkHeader, lastIndex,  iterator - lastIndex).strip().toLowerCase(), null);
            lastIndex = iterator + 1;
            return;
        }
        if (ch == ',') {
            params.put(String.valueOf(linkHeader, lastIndex,  iterator - lastIndex).strip().toLowerCase(), null);

            links.add(create(targetUri, params));
            targetUri = null;
            params = new HashMap<>();
            
            state = State.INIT;
        }
    }
    
    private final void parseParameters(final char ch, final int iterator) {
        if (Character.isSpaceChar(ch) || ch == '\t') {
            return;
        }
        if (ch != ';') {
            links.add(create(targetUri, params));
            state = State.UNEXPECTED;
            return;
        }
        state = State.PARAM_NAME;
        lastIndex = iterator + 1;
    }

    private final void initParser(final char ch, final int iterator) {
        if (Character.isSpaceChar(ch) || ch == '\t') {
            return;
        }
        if (ch == '<') {
            state = State.URI_REF;
            lastIndex = iterator + 1;
            return;
        }
        state = State.UNEXPECTED;
    }
    
    private final void parseUri(final char ch, final int iterator, final URI baseUri) {
        if (ch != '>') {
            return;
        }
        targetUri = UriResolver.resolve(baseUri, String.valueOf(linkHeader, lastIndex,  iterator - lastIndex).stripTrailing());
        state = State.PARAMS;
    }
    
    private final void escape(final char ch) {
        stringValue.append(ch);
        state = State.STRING_VALUE;
    }
}