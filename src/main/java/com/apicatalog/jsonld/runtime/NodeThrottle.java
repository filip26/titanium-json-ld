package com.apicatalog.jsonld.runtime;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.runtime.Execution.EventProcessor;
import com.apicatalog.jsonld.runtime.Execution.EventType;

/**
 * A counter that tracks the number of processed nodes and enforces a maximum
 * limit.
 * <p>
 * This class is typically used within JSON-LD processing to prevent excessive
 * recursion or processing of an unbounded number of nodes. When the limit is
 * exceeded, a {@link com.apicatalog.jsonld.JsonLdException} is thrown.
 * </p>
 * 
 * @since 2.0.0
 */
public class NodeThrottle implements EventProcessor {

    private final int maxNodes;
    private int counter;

    public NodeThrottle(int maxNodes) {
        this.maxNodes = maxNodes;
        this.counter = 0;
    }

    @Override
    public void onEvent(EventType type, String key, String value) throws JsonLdException {
        // FIXME count only begins
        if (++counter >= maxNodes) {
            // TODO add ErrorCode.MAX_NODES_LIMIT_EXCEEDED
            throw new JsonLdException(ErrorCode.UNSPECIFIED);
        }
    }
}
