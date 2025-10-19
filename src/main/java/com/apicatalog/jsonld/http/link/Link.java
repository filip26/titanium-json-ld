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
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

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
        LinkAttributes attributes) {

    public static final Collection<Link> of(final String linkHeader) {
        return of(linkHeader, null);
    }

    public static final Collection<Link> of(final String linkHeader, final URI baseUri) {
        return new LinkHeaderParser(baseUri).parse(Objects.requireNonNull(linkHeader));
    }
}
