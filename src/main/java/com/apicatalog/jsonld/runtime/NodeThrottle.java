/*
 * Copyright 2020-2026 the original author or authors.
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
package com.apicatalog.jsonld.runtime;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.runtime.Execution.TermValueConsumer;

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
