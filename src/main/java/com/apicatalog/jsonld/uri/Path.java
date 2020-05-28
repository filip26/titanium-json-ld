package com.apicatalog.jsonld.uri;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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
    
    public static final Path of(final String path) {

        final boolean relative = !path.startsWith("/");
        
        final List<String> segments = new ArrayList<>(
                                        Arrays.asList(
                                                (relative
                                                    ? path
                                                    : path.substring(1)
                                                    )
                                                .split("\\/")
                                                )
                                        );
        final String last = (path.length() > 1 && path.endsWith("/"))
                                ?  null
                                : segments.remove(segments.size() - 1);
    
        return new Path(segments, (last == null || last.isBlank()) ? null : last, relative);
    }
    
    public Path relativize(final String base) {
        return relativize(Path.of(base));
    }
    
    public Path relativize(final Path base) {

        if (segments.isEmpty() && base.segments.isEmpty()) {
            if (Objects.equals(last, base.last)) {
                return new Path(EMPTY.segments, null, !base.relative);
            } 
            return new Path(EMPTY.segments, last, false);
        }

        if (base.isEmpty()) {
            return this;
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
            return new Path(EMPTY.segments, last, segments.size() > 0);
        }

        if (leftIndex == 0) {
            return this;
        }

        if (leftIndex >= base.segments.size()) {
            return new Path(segments.subList(leftIndex, segments.size()), last, true);
        }
        
        int rightIndex = 0;
        
        for (; rightIndex < Math.min(segments.size(), base.segments.size()) - leftIndex; rightIndex++) {
            if (!segments.get(segments.size() - rightIndex - 1).equals(base.segments.get(base.segments.size() - rightIndex - 1))) {
                break;
            }
        }

        //TODO
        //System.out.println("2 > " + leftIndex + ", " + rightIndex);

        return EMPTY;
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
                    .concat(segments.stream().collect(Collectors.joining("/")))
                    .concat(segments.isEmpty() ? "" : "/")
                    .concat(last != null ? last : "")
                ;
    }    
}
