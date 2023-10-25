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
package no.hasmac.jsonld.uri;

import java.net.URI;
import java.util.Objects;

public final class UriRelativizer {

    private UriRelativizer() {
    }

    public static String relativize(final URI base, final String uri) {

        if (base == null) {
            return uri;
        }

        return relativize(base, URI.create(uri));
    }

    public static String relativize(final URI base, final URI uri) {

        if (base == null || !base.isAbsolute() || !uri.isAbsolute()) {
            return uri.toString();
        }

        if (!Objects.equals(base.getScheme(), uri.getScheme())) {
            return uri.toString();
        }

        if (!Objects.equals(base.getAuthority(), uri.getAuthority())) {
            return uri.toString();
        }

        final Path uriPath = Path.of(uri.getPath());
        final Path basePath = Path.of(base.getPath());

        final Path path = uriPath.relativize(basePath);

        if (path.isNotEmpty()) {
            return UriUtils.recompose( path.toString() , uri.getQuery(), uri.getFragment());
        }

        if (!Objects.equals(base.getQuery(), uri.getQuery())) {
            return UriUtils.recompose( uri.getQuery(), uri.getFragment());
        }

        if (!Objects.equals(base.getFragment(), uri.getFragment())) {
            return UriUtils.recompose( uri.getFragment());
        }

        return uriPath.getLeaf() != null
                    ? uriPath.getLeaf()
                    : "./";
    }
}
