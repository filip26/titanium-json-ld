package com.apicatalog.jsonld.http;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc6838#section-4.2">Media Type Specifications - </a>
 *
 */
final class MediaTypeParser {

    private enum State { INIT, TYPE, SUBTYPE, PARAMS, PARAM_NAME, PARAM_VALUE, STRING_VALUE, LITERAL_VALUE, ESCAPE }
    
    private final char[] input;
    
    public MediaTypeParser(String input) {
        this.input = input.toCharArray();
    }
    
    public MediaType parse() {
        
        State state = State.INIT;

        String type = null;
        String subtype = null;
        String paramName = null;
        
        StringBuilder stringValue = new StringBuilder();
        
        Map<String, String> params = new HashMap<>();
        
        int index = -1;
        
        for (int i=0; i < input.length; i++) {
    
            char ch = input[i];
            
            switch (state) {
            case INIT:
                if (Character.isSpaceChar(ch) || ch == '\t') {
                    break;
                }
                state = State.TYPE;
                index = i;
                break;
                
            case TYPE:
                if (ch != '/') {
                    break;
                }
                type = String.valueOf(input, index,  i - index).strip();
                state = State.SUBTYPE;
                index = i + 1;
                break;

            case SUBTYPE:
                if (ch != ';') {
                    break;
                }
                subtype = String.valueOf(input, index,  i - index).strip();
                state = State.PARAM_NAME;
                index = i + 1;
                break;

            case PARAMS:
                if (Character.isSpaceChar(ch) || ch == '\t') {
                    break;
                }
                if (ch != ';') {
                    return new MediaType(type, subtype, params);
                }
                state = State.PARAM_NAME;
                index = i + 1;
                break;
                
            case PARAM_NAME:
                if (ch == '=') {
                    paramName = String.valueOf(input, index,  i - index).strip().toLowerCase();
                    state = State.PARAM_VALUE;
                    break;
                }
                if (ch == ';') {
                    params.put(String.valueOf(input, index,  i - index).strip().toLowerCase(), null);
                    index = i + 1;
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
                    
                    params.put(paramName, String.valueOf(input, index,  i - index).strip());
                    index = i + 1;
                    paramName = null;
                    state = State.PARAM_NAME;
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
        case SUBTYPE:
            if (index < input.length) {
                subtype = String.valueOf(input, index,  input.length - index).strip().toLowerCase();
            }
            break;
            
        case PARAM_NAME:
            if (index < input.length) {
                paramName = String.valueOf(input, index,  input.length - index).strip().toLowerCase();
            }
            break;
            
        case LITERAL_VALUE:
            if (index < input.length) {
                params.put(paramName, String.valueOf(input, index,  input.length - index).strip());
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
        if (type == null || subtype == null) {
            return null;
        }
        return new MediaType(type, subtype, params);
    }    
}
