package com.apicatalog.jsonld.runtime;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.function.BiConsumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.runtime.Execution.TermValueConsumer;

public final class TermMapCollector implements TermValueConsumer {

    private final Deque<Object> path = new ArrayDeque<>();
    private final BiConsumer<String, Object> consumer;

    public TermMapCollector(BiConsumer<String, Object> consumer) {
        this.consumer = consumer;
    }

    @Override
    public void term(EventType type, String key, String value) throws JsonLdException {
        switch (type) {
        case BEGIN_LIST:
            if (key != null) {
                path.push(key);
            }
            return;

        case END_LIST:
            if (key != null) {
                path.pop();
            }
            return;

        case BEGIN_MAP:
            path.push(TypeMapCollector.escapeJsonPointerSegment(key));
            return;

        case END_MAP:
            path.pop();
            return;

        case TERM_KEY:
            if (path.isEmpty()) {
                consumer.accept("/" + TypeMapCollector.escapeJsonPointerSegment(key), value);
                return;

            }
            var pointer = new ArrayList<String>(path.size());
            path.stream().map(Object::toString).forEach(pointer::add);
            Collections.reverse(pointer);
            pointer.add(TypeMapCollector.escapeJsonPointerSegment(key));

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
}
