package com.apicatalog.jsonld.lang;

import java.util.Objects;
import java.util.stream.IntStream;

import com.apicatalog.jsonld.rdf.nq.impl.RdfGrammar;

/**
 * 
 * @see <a href="https://www.w3.org/TR/curie/">A syntax for expressing Compact
 *      URIs</a>
 *
 */
public final class BlankNode {

    private final String suffix;

    private BlankNode(final String suffix) {
        this.suffix = suffix;
    }

    public static BlankNode create(String value) {
        if (value != null && value.startsWith("_:")) {
            final String suffix = value.substring("_:".length());

            return new BlankNode(suffix);            
        }

        throw new IllegalArgumentException();
    }

    public String getSuffix() {
        return suffix;
    }

    @Override
    public String toString() {
        return "_:".concat(suffix);
    }

    @Override
    public int hashCode() {
        return suffix.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        BlankNode other = (BlankNode) obj;
        return Objects.equals(suffix, other.suffix);
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
                        && RdfGrammar.IS_DIGIT.negate().test(chars[2]))
                || chars[chars.length - 1] == '.'
                        )  {
            return false;
        }
        
        if (chars.length == 3) {
            return true;
        }
        
        return IntStream.range(3, chars.length).map(i -> chars[i]).allMatch(RdfGrammar.IS_PN_CHARS.or(ch -> ch == '.'));        
    }    
}
