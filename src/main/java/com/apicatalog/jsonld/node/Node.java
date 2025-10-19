package com.apicatalog.jsonld.node;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.NodeAdapter;

public class Node {

    private static final Set<String> GRAPH_ALLOWED_KEYS = Set.of(
            Keywords.GRAPH,
            Keywords.ID,
            Keywords.INDEX,
            Keywords.CONTEXT);

    public static final boolean isNotGraphNode(Object value) {
        return value == null
                || !(value instanceof Map map)
                || !map.keySet().contains(Keywords.GRAPH)
                || !GRAPH_ALLOWED_KEYS.containsAll(map.keySet());
    }
    
    public static final boolean isDefaultNode(Object node, NodeAdapter adapter) {
        return adapter.isMap(node) && adapter.keys(node).contains(Keywords.DEFAULT);
    }
    
    public static Optional<?> findDefaultValue(Object node, NodeAdapter adapter) {
        return adapter.isMap(node)
                            ? Optional.ofNullable(adapter.property(Keywords.DEFAULT, node))
                            : Optional.empty();
    }

}
