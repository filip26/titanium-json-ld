package com.apicatalog.jsonld.runtime;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.runtime.Execution.TermValueConsumer;
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
public final class NodeThrottle implements TermValueConsumer {

    private final int maxNodes;
    private final int maxDepth;

    private int counter;
    private int depth;

    public NodeThrottle(int maxNodes, int maxDepth) {
        this.maxNodes = maxNodes;
        this.maxDepth = maxDepth;
        this.counter = 0;
        this.depth = 0;
    }

    @Override
    public void term(EventType type, String key, String value) throws JsonLdException {

        switch (type) {
        case BEGIN_LIST:
        case BEGIN_MAP:
            if (++depth >= maxDepth) {
                // TODO add ErrorCode.MAX_DEPTH_LIMIT_EXCEEDED
                throw new JsonLdException(ErrorCode.UNSPECIFIED);
            }
            
        case TERM_KEY:
        case UNDEFINED_TERM:
            if (++counter >= maxNodes) {
                // TODO add ErrorCode.MAX_NODES_LIMIT_EXCEEDED
                throw new JsonLdException(ErrorCode.UNSPECIFIED);
            }
            break;

        case END_LIST:
        case END_MAP:
            depth--;
            break;

        default:
            break;
        }
    }
}
