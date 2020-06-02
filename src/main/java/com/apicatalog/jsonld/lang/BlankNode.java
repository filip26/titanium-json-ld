package com.apicatalog.jsonld.lang;

import java.util.Objects;

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

    public static boolean isBlankNode(final String value) {
        return value.startsWith("_:");
    }
}
