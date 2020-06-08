package com.apicatalog.jsonld.lang;

import java.util.stream.IntStream;

import com.apicatalog.rdf.lang.RdfGrammar;

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
     */
    public static boolean isWellFormed(final String label) {

        if (label == null) {
            throw new IllegalArgumentException();
        }
        
        if (label.length() < 3) {
            return false;
        }

        int[] chars = label.codePoints().toArray();

        if (chars[0] != '_' 
                || chars[1] != ':' 
                || (RdfGrammar.IS_PN_CHARS_U.negate().test(chars[2])
                        && RdfGrammar.IS_ASCII_DIGIT.negate().test(chars[2]))
                || chars[chars.length - 1] == '.'
                        )  {
            return false;
        }
        
        if (chars.length == 3) {
            return true;
        }
        
        return IntStream.range(3, chars.length - 1).map(i -> chars[i]).allMatch(RdfGrammar.IS_PN_CHARS.or(ch -> ch == '.'));        
    }
}
