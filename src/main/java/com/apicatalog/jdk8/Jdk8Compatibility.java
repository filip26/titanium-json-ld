package com.apicatalog.jdk8;

import org.apache.commons.lang3.StringUtils;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Supplier;

public interface Jdk8Compatibility {

    static String strip(String s) {
        return StringUtils.strip(s);
    }

    static Boolean isBlank(String s) {
        return StringUtils.isBlank(s);
    }

    static String stripTrailing(String s) {
        return StringUtils.stripEnd(s, null);
    }

    static <T> Boolean isEmpty(Optional<T> o) {
        return !o.isPresent();
    }

    Supplier<NoSuchElementException> noSuchElementException = () -> new NoSuchElementException("Optional is empty");

    static <T> T orElseThrow(Optional<T> o) {
        return o.orElseThrow(noSuchElementException);
    }
}
