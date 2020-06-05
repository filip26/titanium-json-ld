package com.apicatalog.jsonld.lang;

import java.util.Objects;

/**
 * 
 * @see <a href="https://www.w3.org/TR/curie/">A syntax for expressing Compact
 *      URIs</a>
 *
 */
public final class CompactUri {

    private final String prefix;
    private final String suffix;

    private final boolean blank;

    CompactUri(final String prefix, final String suffix, final boolean blank) {
        this.prefix = prefix;
        this.suffix = suffix;
        this.blank = blank;
    }

    public static CompactUri create(String value) {
        final int splitIndex = value.indexOf(':', 1);

        if (splitIndex != -1) {

            final String prefix = value.substring(0, splitIndex);
            final String suffix = value.substring(splitIndex + 1);

            if ((Character.isAlphabetic(prefix.charAt(0)) || prefix.equals("_")) && !suffix.startsWith("/")) {
                return new CompactUri(prefix, suffix, prefix.equals("_"));
            }
        }
        return null;
    }

    public String getPrefix() {
        return prefix;
    }

    public String getSuffix() {
        return suffix;
    }

    public boolean isNotBlank() {
        return !blank;
    }

    @Override
    public String toString() {
        return prefix.concat(":").concat(suffix);
    }

    @Override
    public int hashCode() {
        return Objects.hash(prefix, suffix);
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
        CompactUri other = (CompactUri) obj;
        return Objects.equals(prefix, other.prefix) && Objects.equals(suffix, other.suffix);
    }
}
