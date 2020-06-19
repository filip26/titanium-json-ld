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

    private enum State { INIT, URI_REF, PARAMS, PARAM_NAME, PARAM_VALUE, STRING_VALUE, LITERAL_VALUE, ESCAPE }
    
    private final char[] linkHeader;
    
    public LinkHeaderParser(String linkHeader) {
        this.linkHeader = linkHeader.toCharArray();
    }
    

    public Set<Link> parse(URI baseUri) {
        
        Set<Link> links = new LinkedHashSet<>();
        
        State state = State.INIT;

        String targetUri = null;
        String paramName = null;
        StringBuilder stringValue = new StringBuilder();
        
        Map<String, String> params = new HashMap<>();
        
        int index = -1;
        
        for (int i=0; i < linkHeader.length; i++) {
    
            char ch = linkHeader[i];
            
            switch (state) {
            case INIT:
                if (Character.isSpaceChar(ch) || ch == '\t') {
                    break;
                }
                if (ch == '<') {
                    state = State.URI_REF;
                    index = i + 1;
                    break;
                }
                return links;
                
            case URI_REF:
                if (ch != '>') {
                    break;
                }
                targetUri = UriResolver.resolve(baseUri, String.valueOf(linkHeader, index,  i - index).stripTrailing());
                state = State.PARAMS;
                break;
                
            case PARAMS:
                if (Character.isSpaceChar(ch) || ch == '\t') {
                    break;
                }
                if (ch != ';') {
                    links.add(create(targetUri, params));
                    return links;
                }
                state = State.PARAM_NAME;
                index = i + 1;
                break;
                
            case PARAM_NAME:
                if (ch == '=') {
                    paramName = String.valueOf(linkHeader, index,  i - index).strip().toLowerCase();
                    state = State.PARAM_VALUE;
                    break;
                }
                if (ch == ';') {
                    params.put(String.valueOf(linkHeader, index,  i - index).strip().toLowerCase(), null);
                    index = i + 1;
                    break;                    
                }
                if (ch == ',') {
                    params.put(String.valueOf(linkHeader, index,  i - index).strip().toLowerCase(), null);

                    links.add(create(targetUri, params));
                    targetUri = null;
                    params = new HashMap<>();
                    
                    state = State.INIT;
                    break;
                }
                break;
                
            case PARAM_VALUE:
                if (Character.isSpaceChar(ch) || ch == '\t') {
                    break;
                }

                if (ch == '"') {
                    index = i + 1;
                    state = State.STRING_VALUE;
                    break;
                }
                
                index = i;
                state = State.LITERAL_VALUE;
                break;
            
            case LITERAL_VALUE:
                if (ch == ';') {
                    
                    params.put(paramName, String.valueOf(linkHeader, index,  i - index).strip());
                    index = i + 1;
                    paramName = null;
                    state = State.PARAM_NAME;
                    break;                    
                }
                if (ch == ',') {
                    params.put(paramName, String.valueOf(linkHeader, index,  i - index).strip());
                    links.add(create(targetUri, params));
                    paramName = null;
                    targetUri = null;
                    params = new HashMap<>();
                    state = State.INIT;
                    break;
                }
                break;                

            case STRING_VALUE:
                
                if (ch == '"') {
                    params.put(paramName, stringValue.toString());
                    stringValue.setLength(0);
                    paramName = null;
                    state = State.PARAMS;
                    break;
                }
                if (ch == '\\') {
                    state = State.ESCAPE;
                    break;
                }
                stringValue.append(ch);
                break;
            
            case ESCAPE:
                stringValue.append(ch);
                state = State.STRING_VALUE;
                break;
            }            
        }

        switch (state) {
        case PARAM_NAME:
            if (index < linkHeader.length) {
                paramName = String.valueOf(linkHeader, index,  linkHeader.length - index).strip().toLowerCase();
            }
            break;
            
        case LITERAL_VALUE:
            if (index < linkHeader.length) {
                params.put(paramName, String.valueOf(linkHeader, index,  linkHeader.length - index).strip());
                paramName = null;
            }
            break;
            
        default:
            break;
        }

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
    
    private static final Link create(String uri, Map<String, String> params) {
        
        Set<String> rel = Collections.emptySet();
        
        if (params.containsKey("rel") && params.get("rel") != null) {
            rel = new HashSet<>(Arrays.asList(params.get("rel").split("(\\s\\t)+")));
        }

        return new Link(URI.create(uri), rel, params);
    }
    
}
