package com.apicatalog.jdk8;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Supplier;

public interface Jdk8Compatibility {

    Supplier<NoSuchElementException> noSuchElementException = () -> new NoSuchElementException("Optional is empty");

    static String strip(String str) {
        return stripTrailing(stripLeading(str));
    }

    static Boolean isBlank(String cs) {
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

    static String stripLeading(final String str) {
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

    static String stripTrailing(String str) {
        int end = length(str);
        if (end == 0) {
            return str;
        }
        while (end != 0 && Character.isWhitespace(str.charAt(end - 1))) {
            end--;
        }
        return str.substring(0, end);
    }

    static int length(final CharSequence cs) {
        return cs == null ? 0 : cs.length();
    }

    static <T> Boolean isEmpty(Optional<T> o) {
        return !o.isPresent();
    }
}
