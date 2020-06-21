package com.apicatalog.jsonld.http.link;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.http.HttpAlphabet;
import com.apicatalog.jsonld.http.MediaType;
import com.apicatalog.jsonld.uri.UriResolver;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc8288#appendix-B">Appendix B.  Algorithms for Parsing Link Header Fields</a>
 *
 */
final class LinkHeaderParser {

    private static final String REL = "rel";
    private static final String ANCHOR = "anchor";
    private static final String TYPE = "type";
    
    private enum State { INIT, URI_REF, PARAMS, PARAM_NAME, PARAM_VALUE, STRING_VALUE, LITERAL_VALUE, ESCAPE, UNEXPECTED }
    
    private URI baseUri;
    
    private final StringBuilder valueBuilder;
    
    private List<Link> links;
    private State state;

    private URI targetUri;
    private String attributeName;
    private String attributeValue;
    private Map<String, List<LinkAttribute>> attributes;
    
    public LinkHeaderParser(final URI baseUri) {
        this.baseUri = baseUri;
        this.valueBuilder = new StringBuilder();
    }

    public List<Link> parse(String httpLink) {
        
        resetState(baseUri);
    
        final char[] linkHeader = httpLink.toCharArray();
        
        for (int i=0; i < linkHeader.length; i++) {
    
            final char ch = linkHeader[i];
        
            switch (state) {
            case INIT:
                initParser(ch);
                break;
                
            case URI_REF:
                parseTargetUri(ch);
                break;
                
            case PARAMS:
                parseParameters(ch);
                break;
                
            case PARAM_NAME:
                parseParamName(ch);
                break;
                
            case PARAM_VALUE:
                parseParamValue(ch);
                break;
            
            case LITERAL_VALUE:
                parseLiteral(ch);
                break;                

            case STRING_VALUE:
                parseString(ch);
                break;
            
            case ESCAPE:
                escape(ch);
                break;
                
            default:
                addParameter();
                addLink();
                return links;
            }            
        }

        return sweep();
    }

    private final List<Link> sweep() {
        switch (state) {
        case PARAM_NAME:
            if (valueBuilder.length() > 0) {
                attributeName = valueBuilder.toString().stripTrailing();
            }
            break;
                        
        case LITERAL_VALUE:
            if (valueBuilder.length() > 0) {
                attributeValue = valueBuilder.toString().stripTrailing();
            }
            break;
            
        case UNEXPECTED:
            return links;
            
        default:
            break;
        }
        
        addParameter();
        addLink();
        
        return links;
    }
    
    private final void addLink() {
        if (targetUri != null) {
            
            Set<String> rel = Collections.emptySet();
            URI context = null;
            MediaType type = null;
            
            if (attributes.containsKey(REL) && attributes.get(REL) != null) {
                rel = new HashSet<>(Arrays.asList(attributes.get(REL).get(0).value().split("[\\s\\t]+")));
                attributes.remove(REL);
            }
            if (attributes.containsKey(ANCHOR) && attributes.get(ANCHOR) != null) {
                context = URI.create(UriResolver.resolve(baseUri, attributes.get(ANCHOR).get(0).value()));
                attributes.remove(ANCHOR);
            }
            if (attributes.containsKey(TYPE) && attributes.get(TYPE) != null) {
                type = MediaType.valueOf(attributes.get(TYPE).get(0).value());
                if (type != null) {
                    attributes.remove(TYPE);
                }
            }
            
            links.add(new Link(context, targetUri, rel, type, new LinkAttributes(attributes)));

            targetUri = null;
            attributes = new LinkedHashMap<>();
        }
    }
    
    private final void addParameter() {
        if (attributeName != null) {
            if (attributeValue != null) {
                attributes
                        .computeIfAbsent(attributeName, l -> new LinkedList<>())
                        .add(new LinkAttribute(attributeName, attributeValue));
                
                attributeValue = null;
                
            } else {

                attributes
                        .computeIfAbsent(attributeName, l -> new LinkedList<>())
                        .add(new LinkAttribute(attributeName));
            }
            
            attributeName = null;
        }
    }

    private final void resetState(URI baseUri) {
        this.baseUri = baseUri;
        this.links = new LinkedList<>();
        this.attributes = new LinkedHashMap<>();
        this.state = State.INIT;
        
        this.targetUri = null;
        this.attributeName = null;
        this.attributeValue = null;
    }

    private final void initParser(final char ch) {
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (ch == '<') {
            valueBuilder.setLength(0);
            state = State.URI_REF;
            return;
        }
        state = State.UNEXPECTED;
    }
    
    private final void parseTargetUri(final char ch) {
        if (ch != '>') {
            if (valueBuilder.length() > 0 || HttpAlphabet.WHITESPACE.negate().test(ch)) {
                valueBuilder.append(ch);
            }
            return;
        }
        
        targetUri = URI.create(UriResolver.resolve(baseUri, valueBuilder.toString().stripTrailing()));
        state = State.PARAMS;
    }
    
    private final void parseParameters(final char ch) {
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (ch == ',') {
            addLink();
            state = State.INIT;
            return;
        }
        if (ch == ';') {
            valueBuilder.setLength(0);
            state = State.PARAM_NAME;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void parseParamName(final char ch) {
        if (ch == '=') {
            attributeName = valueBuilder.toString().stripTrailing();
            valueBuilder.setLength(0);
            state = State.PARAM_VALUE;
            return;
        }
        if (ch == ';') {
            attributeName = valueBuilder.toString().stripTrailing();
            valueBuilder.setLength(0);
            addParameter();
            return;
        }
        if (ch == ',') {
            attributeName = valueBuilder.toString().stripTrailing();
            addParameter();
            addLink();            
            state = State.INIT;
            return;
        }
        if (valueBuilder.length() > 0 || HttpAlphabet.WHITESPACE.negate().test(ch)) {
            valueBuilder.append(ch);
        }
    }

    private final void parseParamValue(final char ch) {
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (ch == '"') {
            state = State.STRING_VALUE;
            return;
        }
        if (valueBuilder.length() > 0 || HttpAlphabet.WHITESPACE.negate().test(ch)) {
            valueBuilder.append(ch);
        }
        state = State.LITERAL_VALUE;
    }
    

    private final void parseString(final char ch) {
        if (ch == '"') {
            attributeValue = valueBuilder.toString().stripTrailing();
            addParameter();
            state = State.PARAMS;
            return;
        }
        if (ch == '\\') {
            state = State.ESCAPE;
            return;
        }
        if (valueBuilder.length() > 0 || HttpAlphabet.WHITESPACE.negate().test(ch)) {
            valueBuilder.append(ch);
        }
    }
    
    private final void parseLiteral(final char ch) {

        if (ch == ';') {
            attributeValue = valueBuilder.toString().stripTrailing();
            valueBuilder.setLength(0);
            addParameter();
            state = State.PARAM_NAME;
            return;
            
        } else if (ch == ',') {
            attributeValue = valueBuilder.toString().stripTrailing();
            addParameter();
            addLink();
            state = State.INIT;
            return;
        }
        if (valueBuilder.length() > 0 || HttpAlphabet.WHITESPACE.negate().test(ch)) {
            valueBuilder.append(ch);
        }
    }
        
    private final void escape(final char ch) {
        if (ch == 't') {
            valueBuilder.append('\t');
        } else {
            valueBuilder.append(ch);    
        }
        
        state = State.STRING_VALUE;
    }
}