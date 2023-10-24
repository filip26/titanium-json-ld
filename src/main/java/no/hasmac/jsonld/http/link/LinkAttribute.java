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
package no.hasmac.jsonld.http.link;

import java.nio.charset.Charset;
import java.util.Objects;
import java.util.Optional;

/**
 * A read-only view of Link attribute.
 *
 */
public final class LinkAttribute {

    private final String name;
    private final String value;

    private final String languageTag;

    protected LinkAttribute(final String name) {
        this(name, name, null);
    }

    protected LinkAttribute(final String name, final String value) {
        this(name, value, null);
    }

    protected LinkAttribute(final String name, final String value, final String languageTag) {
        this.name = name;
        this.value = value;
        this.languageTag = languageTag;
    }

    public Optional<String> languageTag() {
        return Optional.ofNullable(languageTag);
    }

    public String name() {
        return name;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        if (value == null) {
            return name;
        }

        final StringBuilder builder = new StringBuilder();

        builder.append(name);
        builder.append('=');

        if (languageTag != null) {
            builder.append(Charset.defaultCharset().name());
            builder.append('\'');
            builder.append(languageTag);
            builder.append('\'');
        }
        builder.append(value);

        return builder.toString();
    }

    @Override
    public int hashCode() {
        return Objects.hash(languageTag, name, value);
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
        LinkAttribute other = (LinkAttribute) obj;
        return Objects.equals(languageTag, other.languageTag) && Objects.equals(name, other.name)
                && Objects.equals(value, other.value);
    }
}
