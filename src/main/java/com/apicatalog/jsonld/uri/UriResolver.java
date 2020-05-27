package com.apicatalog.jsonld.uri;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc3986#section-5.2">Relative
 *      Resolution</a>
 *
 */
public final class UriResolver {

    UriResolver() {
    }

    public static String resolve(URI base, String relative) {

        if (base == null) {
            return relative;
        }

        //FIXME hack
        if (relative.endsWith(":")) {
            relative += ".";
        }
        
        URI components = URI.create(relative);

        String basePath = base.getPath();
        String componentPath = components.getPath();
        
        // hack
        if (basePath == null && base.getSchemeSpecificPart() != null) {
            basePath = base.getSchemeSpecificPart();
        }
        if (componentPath == null && components.getSchemeSpecificPart() != null) {
            componentPath = components.getSchemeSpecificPart();
        }
        
        String scheme = null;
        String authority = null;
        String path = null;
        String query = null;

        if (isDefined(components.getScheme())) {
            scheme = components.getScheme();
            authority = components.getAuthority();
            path = removeDotSegments(componentPath);
            query = components.getQuery();

        } else {
            
            if (isDefined(components.getAuthority())) {
                authority = components.getAuthority();
                path = removeDotSegments(componentPath);
                query = components.getQuery();

            } else {

                if (isNotDefined(componentPath)) {
                    path = basePath;

                    if (isDefined(components.getQuery())) {
                        query = components.getQuery();

                    } else {
                        query = base.getQuery();
                    }

                } else {
                    if (componentPath.startsWith("/")) {
                        path = removeDotSegments(componentPath);

                    } else {
                        path = removeDotSegments(merge(basePath, componentPath));
                    }
                    query = components.getQuery();
                }
                authority = base.getAuthority();
            }
            scheme = base.getScheme();            
        }

        return recomposition(scheme, authority, path, query, components.getFragment());
    }

    /**
     * 
     * @see <a href="https://tools.ietf.org/html/rfc3986#section-5.2.4">Remove Dot
     *      Segments</a>
     * 
     * @param basePath
     * @param path
     * @return
     */
    static String removeDotSegments(final String path) {

        if (isNotDefined(path)) {
            return "";
        }

        String input = path;
        List<String> output = new ArrayList<>();

        while (!input.isBlank()) {

            // A.
            if (input.startsWith("../")) {
                input = input.substring(3);

            } else if (input.startsWith("./")) {
                input = input.substring(2);

                // B.
            } else if (input.startsWith("/./")) {
                input = "/".concat(input.substring(3));

            } else if (input.equals("/.")) {
                input = "/";

                // C.
            } else if (input.startsWith("/../")) {
                input = "/".concat(input.substring(4));
                if (!output.isEmpty()) {
                    output.remove(output.size() - 1);
                }

            } else if (input.equals("/..")) {
                input = "/";
                if (!output.isEmpty()) {
                    output.remove(output.size() - 1);
                }

                // D.
            } else if (input.equals("..") || input.equals(".")) {
                input = "";

                // E.
            } else {
                int nextSlashIndex = input.indexOf('/', 1);

                if (nextSlashIndex != -1) {
                    output.add(input.substring(0, nextSlashIndex));
                    input = input.substring(nextSlashIndex);

                } else {
                    output.add(input);
                    input = "";
                }
            }
        }

        return output.stream().collect(Collectors.joining());
    }

    /**
     * 
     * @see <a href="https://tools.ietf.org/html/rfc3986#section-5.2.3">Merge
     *      Paths</a>
     * 
     * @param basePath
     * @param path
     * @return
     */
    static String merge(String basePath, String path) {
        if (isNotDefined(basePath)) {
            return "/".concat(path);
        }

        int rightMostSlash = basePath.lastIndexOf('/');

        if (rightMostSlash == -1) {
            return path;
        }

        return basePath.substring(0, rightMostSlash + 1).concat(path);
    }

    static String recomposition(String scheme, String authority, String path, String query, String fragment) {

        StringBuilder builder = new StringBuilder();

        if (isDefined(scheme)) {
            builder.append(scheme);
            builder.append(":");
        }
        if (isDefined(authority)) {
            builder.append("//");
            builder.append(authority);
        }
        if (isDefined(path)) {
            builder.append(path);
        }
        if (isDefined(query)) {
            builder.append('?');
            builder.append(query);
        }
        if (isDefined(fragment)) {
            builder.append('#');
            builder.append(fragment);
        }
        return builder.toString();
    }

    static boolean isDefined(String value) {
        return value != null && !value.isBlank();
    }

    static boolean isNotDefined(String value) {
        return value == null || value.isBlank();
    }
}
