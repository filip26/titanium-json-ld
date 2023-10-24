/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package no.hasmac.jsonld.lang;

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

    private CompactUri(final String prefix, final String suffix, final boolean blank) {
        this.prefix = prefix;
        this.suffix = suffix;
        this.blank = blank;
    }

    public static CompactUri create(String value) {
        final int splitIndex = value.indexOf(':', 1);

        if (splitIndex != -1) {

            final String prefix = value.substring(0, splitIndex);
            final String suffix = value.substring(splitIndex + 1);

            if (!suffix.startsWith("/") && ("_".equals(prefix) || Character.isAlphabetic(prefix.charAt(0)))) {
                return new CompactUri(prefix, suffix, "_".equals(prefix));
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
