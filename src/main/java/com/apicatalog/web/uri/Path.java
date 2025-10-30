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
package com.apicatalog.web.uri;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

public record Path(
        List<String> segments,
        String leaf,
        boolean relative) {

    public static final Path EMPTY = new Path(new ArrayList<>(), null, true);

    private static final Pattern SEGMENTS_DEL_RE = Pattern.compile("/");

    public Path {
        segments = List.copyOf(segments);
    }

    public static final Path of(final String path) {

        final var relative = !path.startsWith("/");

        //TODO better
        final var segments = new ArrayList<String>(
                List.of(SEGMENTS_DEL_RE.split(
                        (relative
                                ? path
                                : path.substring(1)))));

        final var leaf = (path.length() > 1 && path.endsWith("/"))
                ? null
                : segments.remove(segments.size() - 1);

        return new Path(segments, (leaf == null || leaf.isBlank()) ? null : leaf, relative);
    }

    public Path relativize(final String base) {
        return relativize(Path.of(base));
    }

    public Path relativize(final Path base) {

        if (segments.isEmpty() && base.segments.isEmpty()) {
            if (Objects.equals(leaf, base.leaf)) {
                return new Path(EMPTY.segments, null, !base.relative);
            }
            return new Path(EMPTY.segments, leaf, !relative && !base.relative);
        }

        if (base.segments.isEmpty() && base.leaf == null) {
            return new Path(segments, leaf, !base.relative && !relative);
        }

        int leftIndex = 0;

        for (; leftIndex < Math.min(segments.size(), base.segments.size()); leftIndex++) {
            if (!segments.get(leftIndex).equals(base.segments.get(leftIndex))) {
                break;
            }
        }

        if (leftIndex == segments.size() && leftIndex == base.segments.size()) {
            if (Objects.equals(leaf, base.leaf)) {
                return EMPTY;
            }
            return new Path(EMPTY.segments, leaf, !segments.isEmpty());
        }

        if (leftIndex >= base.segments.size()) {

            if ((segments.size() - leftIndex == 1) && segments.get(leftIndex).equals(base.leaf)) {
                return new Path(Arrays.asList("."), leaf, true);
            }

            return new Path(segments.subList(leftIndex, segments.size()), leaf, true);
        }

        int rightIndex = 0;

        List<String> diff = new ArrayList<>();

        for (; rightIndex < Math.min(segments.size(), base.segments.size()) - leftIndex; rightIndex++) {
            if (!segments.get(segments.size() - rightIndex - 1).equals(base.segments.get(base.segments.size() - rightIndex - 1))) {
                break;
            }
            diff.add(".."); // TOD ?!?
        }
        for (int i = 0; i < (base.segments.size() - leftIndex - rightIndex); i++) {
            diff.add("..");
        }

        for (int i = 0; i < (segments.size() - leftIndex - rightIndex); i++) {
            diff.add(segments.get(i + leftIndex));
        }

        return new Path(diff, Objects.equals(leaf, base.leaf) ? null : leaf, true);
    }

    public boolean isEmpty() {
        return segments.isEmpty() && leaf == null && !relative;
    }

    public boolean isNotEmpty() {
        return !segments.isEmpty() || leaf != null || !relative;
    }

    public boolean isAbsolute() {
        return !relative;
    }

    @Override
    public String toString() {
        return (relative
                ? ""
                : "/")
                .concat(String.join("/", segments))
                .concat(segments.isEmpty() ? "" : "/")
                .concat(leaf != null ? leaf : "");
    }

}
