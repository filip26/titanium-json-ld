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
package com.apicatalog.jsonld.http.link;

import java.net.URI;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.http.media.MediaType;

/**
 *
 * @see <a href="https://tools.ietf.org/html/rfc8288">Web Linking</a>
 *
 */
public record Link(
        URI context,
        URI target,
        Set<String> relations,
        MediaType type,
        Map<String, List<Attribute>> attributeMap) {

    public Link(URI context, URI target, Set<String> relations, MediaType type, Map<String, List<Attribute>> attributeMap) {
        this.context = context;
        this.target = target;
        this.relations = relations != null
                ? relations
                : Collections.emptySet();
        this.type = type;
        this.attributeMap = attributeMap != null
                ? attributeMap
                : Collections.emptyMap();
    }

    public static final Collection<Link> of(final String linkHeader) {
        return of(linkHeader, null);
    }

    public static final Collection<Link> of(final String linkHeader, final URI baseUri) {
        return new LinkHeaderParser(baseUri).parse(Objects.requireNonNull(linkHeader));
    }

    public List<Attribute> attributes(final String attributeName) {
        return attributeMap.containsKey(attributeName)
                ? attributeMap.get(attributeName)
                : Collections.emptyList();
    }

    public List<Attribute> attributes() {
        return attributeMap.values().stream().flatMap(Collection::stream).collect(Collectors.toList());
    }

    public Optional<Attribute> findFirstAttribute(final String attributeName) {
        return Optional.ofNullable(attributeMap.get(attributeName)).map(attrs -> attrs.get(0));
    }

    public Set<String> attributeNames() {
        return attributeMap.keySet();
    }

    /**
     * A read-only view of Link attribute.
     *
     */
    public static record Attribute(String name, String value, String languageTag) {

        public Attribute(final String name) {
            this(name, name, null);
        }

        public Attribute(final String name, final String value) {
            this(name, value, null);
        }

        public Attribute(final String name, final String value, final String languageTag) {
            this.name = Objects.requireNonNull(name);
            this.value = Objects.requireNonNull(value);
            this.languageTag = languageTag;
        }

        @Override
        public String toString() {
            if (value == null) {
                return name;
            }

            final var builder = new StringBuilder()
                    .append(name)
                    .append('=');

            if (languageTag != null) {
                builder
                        .append(Charset.defaultCharset().name())
                        .append('\'')
                        .append(languageTag)
                        .append('\'');
            }
            return builder
                    .append(value)
                    .toString();
        }
    }
}
