package com.apicatalog.jsonld.runtime;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.Objects;
import java.util.function.BiConsumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.runtime.Execution.EventProcessor;

public final class TypeMapCollector implements EventProcessor {

    private final Deque<Object> path = new ArrayDeque<>();
    private final BiConsumer<String, Object> consumer;

    public TypeMapCollector(BiConsumer<String, Object> consumer) {
        this.consumer = consumer;
    }

    @Override
    public void onEvent(EventType type, String key, String value) throws JsonLdException {

        switch (type) {
        case onBeginList:
            if (key != null) {
                path.push(key);
            }
            return;

        case onEndList:
            if (key != null) {
                path.pop();
            }
            return;

        case onBeginMap:
            path.push(escapeJsonPointerSegment(key));
            return;

        case onEndMap:
            path.pop();
            return;

        case onTypeKey:
            if (path.isEmpty()) {
                consumer.accept("/" + escapeJsonPointerSegment(key), value);
                return;
            }

            var pointer = new ArrayList<String>(path.size());
            path.stream().map(Object::toString).forEach(pointer::add);
            Collections.reverse(pointer);
            pointer.add(escapeJsonPointerSegment(key));

            consumer.accept("/" + String.join("/", pointer), value);

            if (path.peek() instanceof Integer order) {
                path.pop();
                path.push(order + 1);
            }

            return;

        default:
            return;
        }
    }
    
    /** Escape a single reference token (RFC6901): ~ -> ~0 and / -> ~1 */
    protected static String escapeJsonPointerSegment(String s) {
        Objects.requireNonNull(s, "segment");
        int n = s.length();
        StringBuilder sb = new StringBuilder(n + 4); // small growth room
        for (int i = 0; i < n; ++i) {
            char c = s.charAt(i);
            if (c == '~')
                sb.append("~0");
            else if (c == '/')
                sb.append("~1");
            else
                sb.append(c);
        }
        return sb.toString();
    }
}
