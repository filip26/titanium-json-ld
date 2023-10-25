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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import no.hasmac.jsonld.StringUtils;

public final class Path {

    public static final Path EMPTY = new Path(new ArrayList<>(), null, true);

    private final List<String> segments;
    private final String last;
    private final boolean relative;

    private Path(final List<String> segments, String last, final boolean relative) {
        this.segments = segments;
        this.last = last;
        this.relative = relative;
    }

    public static Path of(final String path) {

        final boolean relative = !path.startsWith("/");

        final List<String> segments = new ArrayList<>(
                                        Arrays.asList(
                                                (relative
                                                    ? path
                                                    : path.substring(1)
                                                    )
                                                .split("/")
                                                )
                                        );
        final String last = (path.length() > 1 && path.endsWith("/"))
                                ?  null
                                : segments.remove(segments.size() - 1);

        return new Path(segments, (last == null || StringUtils.isBlank(last)) ? null : last, relative);
    }

    public Path relativize(final String base) {
        return relativize(Path.of(base));
    }

    public Path relativize(final Path base) {

        if (segments.isEmpty() && base.segments.isEmpty()) {
            if (Objects.equals(last, base.last)) {
                return new Path(EMPTY.segments, null, !base.relative);
            }
            return new Path(EMPTY.segments, last, !relative && !base.relative);
        }

        if (base.segments.isEmpty() && base.last == null) {
            return new Path(segments, last, !base.relative && !relative);
        }

        int leftIndex = 0;

        for (; leftIndex < Math.min(segments.size(), base.segments.size()); leftIndex++) {
            if (!segments.get(leftIndex).equals(base.segments.get(leftIndex))) {
                break;
            }
        }

        if (leftIndex == segments.size() && leftIndex == base.segments.size()) {
            if (Objects.equals(last, base.last)) {
                return EMPTY;
            }
            return new Path(EMPTY.segments, last, !segments.isEmpty());
        }

        if (leftIndex >= base.segments.size()) {

            if ((segments.size() - leftIndex == 1) && segments.get(leftIndex).equals(base.last)) {
                return new Path(Arrays.asList("."), last, true);
            }

            return new Path(segments.subList(leftIndex, segments.size()), last, true);
        }

        int rightIndex = 0;

        List<String> diff = new ArrayList<>();

        for (; rightIndex < Math.min(segments.size(), base.segments.size()) - leftIndex; rightIndex++) {
            if (!segments.get(segments.size() - rightIndex - 1).equals(base.segments.get(base.segments.size() - rightIndex - 1))) {
                break;
            }
            diff.add("..");     //TOD ?!?
        }
        for (int i=0; i < (base.segments.size() - leftIndex - rightIndex); i++) {
            diff.add("..");
        }

        for (int i=0; i < (segments.size() - leftIndex - rightIndex ); i++) {
            diff.add(segments.get(i + leftIndex));
        }

        return new Path(diff, Objects.equals(last, base.last) ? null : last, true);
    }

    public boolean isEmpty() {
        return segments.isEmpty() && last == null && !relative;
    }

    public boolean isNotEmpty() {
        return !segments.isEmpty() || last != null || !relative;
    }

    public boolean isRelative() {
        return relative;
    }

    @Override
    public String toString() {
        return (relative
                    ? ""
                    : "/")
                    .concat(String.join("/", segments))
                    .concat(segments.isEmpty() ? "" : "/")
                    .concat(last != null ? last : "")
                ;
    }

    public String getLeaf() {
        return last;
    }
}
