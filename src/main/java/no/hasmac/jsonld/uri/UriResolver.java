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

import no.hasmac.jsonld.StringUtils;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayDeque;
import java.util.Deque;

/**
 * @see <a href="https://tools.ietf.org/html/rfc3986#section-5.2">Relative
 * Resolution</a>
 */
public final class UriResolver {

    private UriResolver() {
    }

    public static String resolve(final URI base, final String relative) {

        if (base == null) {
            return relative;
        }

        return resolve(base, UriUtils.create(relative));
    }

    public static String resolve(final URI base, final URI relative) {

        if (relative == null) {
            return base != null ? base.toString() : null;
        }

        if (base == null) {
            return relative.toString();
        }

        final String[] components = resolveAsComponents(base, relative);

        return UriUtils.recompose(components[0], components[1], components[2], components[3], components[4]);
    }

    public static URI resolveAsUri(final URI base, final String relative) {

        if (StringUtils.isBlank(relative)) {
            return base;
        }

        if (base == null) {
            return UriUtils.create(relative);
        }

        return resolveAsUri(base, UriUtils.create(relative));
    }

    public static URI resolveAsUri(final URI base, final URI relative) {

        if (relative == null) {
            return base;
        }

        if (base == null) {
            return relative;
        }

        final String[] components = resolveAsComponents(base, relative);

        try {
            if (components[0] != null
                    && components[1] == null
            ) {
                return new URI(components[0], components[2].trim().isEmpty() ? "." : components[2], components[4]);
            }

            return new URI(components[0], components[1], components[2], components[3], components[4]);

        } catch (URISyntaxException e) {
            throw new IllegalStateException(e); // should never happen
        }
    }

    private static String[] resolveAsComponents(final URI base, final URI relative) {

        String basePath = base.getPath();
        String baseAuthority = base.getAuthority();

        String componentPath = relative.getPath();

        // hacks
        if (baseAuthority == null && base.getSchemeSpecificPart().startsWith("///")) {
            baseAuthority = "";
        }
        if (basePath == null && base.getSchemeSpecificPart() != null) {
            basePath = base.getSchemeSpecificPart();
        }

        if (componentPath == null && relative.getSchemeSpecificPart() != null) {
            componentPath = relative.getSchemeSpecificPart();
        }

        final String[] target = new String[5];
        target[4] = relative.getFragment();

        if (relative.getScheme() != null && StringUtils.isNotBlank(relative.getScheme())) {
            target[0] = relative.getScheme();
            target[1] = relative.getAuthority();
            target[2] = removeDotSegments(componentPath);
            target[3] = relative.getQuery();

        } else {

            if (relative.getAuthority() != null && StringUtils.isNotBlank(relative.getAuthority())) {
                target[1] = relative.getAuthority();
                target[2] = removeDotSegments(componentPath);
                target[3] = relative.getQuery();

            } else {

                if (componentPath != null && StringUtils.isNotBlank(componentPath)) {
                    if (componentPath.startsWith("/")) {
                        target[2] = removeDotSegments(componentPath);

                    } else if (basePath != null && StringUtils.isNotBlank(basePath)) {
                        target[2] = removeDotSegments(merge(basePath, componentPath));

                    } else {
                        target[2] = "/".concat(removeDotSegments(componentPath));

                    }
                    target[3] = relative.getQuery();

                } else {
                    target[2] = basePath;

                    if (UriUtils.isDefined(relative.getQuery())) {
                        target[3] = relative.getQuery();

                    } else {
                        target[3] = base.getQuery();
                    }

                }
                target[1] = baseAuthority;
            }
            target[0] = base.getScheme();
        }

        return target;
    }

    /**
     * @param path
     * @return
     * @see <a href="https://tools.ietf.org/html/rfc3986#section-5.2.4">Remove Dot
     * Segments</a>
     */
    private static String removeDotSegments(final String path) {

        if (UriUtils.isNotDefined(path)) {
            return null;
        }

        if (!path.contains(".")) {
            return path;
        }


        String input = path;

        Deque<String> output = new ArrayDeque<>();

        while (StringUtils.isNotBlank(input)) {

            // A.
            if (input.startsWith("../")) {
                input = input.substring(3);

            } else if (input.startsWith("./")) {
                input = input.substring(2);

            // B.
            } else if (input.startsWith("/./")) {
                input = input.substring(2); // we need to leave the "/" at the end of "/./"

            } else if ("/.".equals(input)) {
                input = "/";

            // C.
            } else if (input.startsWith("/../")) {
                input = input.substring(3); // we need to leave the "/" at the end of "/../"
                if (!output.isEmpty()) {
                    output.removeLast();
                }

            } else if ("/..".equals(input)) {
                input = "/";
                if (!output.isEmpty()) {
                    output.removeLast();
                }

            // D.
            } else if ("..".equals(input) || ".".equals(input)) {
                input = "";

            // E.
            } else {
                int nextSlashIndex = input.indexOf('/', 1);

                if (nextSlashIndex != -1) {
                    output.addLast(input.substring(0, nextSlashIndex));
                    input = input.substring(nextSlashIndex);

                } else {
                    output.addLast(input);
                    input = "";
                }
            }
        }

        return String.join("", output);
    }


    /**
     * @param basePath
     * @param path
     * @return
     * @see <a href="https://tools.ietf.org/html/rfc3986#section-5.2.3">Merge
     * Paths</a>
     */
    private static String merge(String basePath, String path) {

        if (UriUtils.isNotDefined(basePath)) {
            return "/".concat(path);
        }

        int rightMostSlash = basePath.lastIndexOf('/');

        if (rightMostSlash == -1) {
            return path;
        }

        return basePath.substring(0, rightMostSlash + 1).concat(path);
    }
}
