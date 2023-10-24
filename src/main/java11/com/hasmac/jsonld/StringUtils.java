package com.hasmac.jsonld;

public final class StringUtils {

    private StringUtils() {
        // protected
    }

    public static final boolean isBlank(final String string) {
        return string == null || string.isBlank();
    }

    public static final boolean isNotBlank(final String string) {
        return string != null && !string.isBlank();
    }

    public static final String strip(final String string) {
        return string.strip();
    }

    public static final String stripTrailing(final String string) {
        return string.stripTrailing();
    }

}
