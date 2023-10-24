package com.hasmac.jsonld;

public final class StringUtils {

    private StringUtils() {
        // protected
    }

    public static boolean isNotBlank(final String string) {
        return !isBlank(string);
    }

    public static String strip(String str) {
        return stripTrailing(stripLeading(str));
    }

    public static Boolean isBlank(String cs) {
        final int strLen = length(cs);
        if (strLen == 0) {
            return true;
        }
        for (int i = 0; i < strLen; i++) {
            if (!Character.isWhitespace(cs.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static String stripLeading(final String str) {
        final int strLen = length(str);
        if (strLen == 0) {
            return str;
        }
        int start = 0;
        while (start != strLen && Character.isWhitespace(str.charAt(start))) {
            start++;
        }
        return str.substring(start);
    }

    public static String stripTrailing(String str) {
        int end = length(str);
        if (end == 0) {
            return str;
        }
        while (end != 0 && Character.isWhitespace(str.charAt(end - 1))) {
            end--;
        }
        return str.substring(0, end);
    }

    public static int length(final CharSequence cs) {
        return cs == null ? 0 : cs.length();
    }
}
