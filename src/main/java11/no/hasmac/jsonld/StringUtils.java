package no.hasmac.jsonld;

public final class StringUtils {

    private StringUtils() {
        // protected
    }

    public static boolean isBlank(final String string) {
        return string == null || string.isBlank();
    }

    public static boolean isNotBlank(final String string) {
        return string != null && !string.isBlank();
    }

    public static String strip(final String string) {
        return string.strip();
    }

    public static String stripTrailing(final String string) {
        return string.stripTrailing();
    }

}
