package com.apicatalog.jsonld.lang;

import java.util.stream.IntStream;

import com.apicatalog.rdf.lang.RdfAlphabet;

/**
 * 
 * @see <a href="https://www.w3.org/TR/curie/">A syntax for expressing Compact
 *      URIs</a>
 *
 */
public final class BlankNode {

    private BlankNode() {
        
    }
    
    public static boolean hasPrefix(final String value) {        
        return value.startsWith("_:");
    }

    /**
     * BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
     * 
     * @see <a href="https://www.w3.org/TR/n-quads/#sec-grammar">N-Quads Grammar</a>
     * 
     * @param blankNodeId to check
     * @return <code>true</code> if the provided string is well formed blank node identifier
     */
    public static boolean isWellFormed(final String blankNodeId) {

        if (blankNodeId == null) {
            throw new IllegalArgumentException();
        }
        
        if (blankNodeId.length() < 3) {
            return false;
        }

        int[] chars = blankNodeId.codePoints().toArray();

        if (chars[0] != '_' 
                || chars[1] != ':' 
                || (RdfAlphabet.PN_CHARS_U.negate().test(chars[2])
                        && RdfAlphabet.ASCII_DIGIT.negate().test(chars[2]))
                || chars[chars.length - 1] == '.'
                        )  {
            return false;
        }
        
        if (chars.length == 3) {
            return true;
        }
        
        return IntStream.range(3, chars.length - 1).map(i -> chars[i]).allMatch(RdfAlphabet.PN_CHARS.or(ch -> ch == '.'));        
    }
}
