package com.apicatalog.jsonld.expansion;

import java.util.Map.Entry;
import java.util.stream.Stream;

import com.apicatalog.jsonld.node.ValueNode;
import com.apicatalog.tree.io.NativeAdapter;
import com.apicatalog.tree.io.NodeType;

public class JsonLdAdapter extends NativeAdapter {

    @Override
    public NodeType type(Object node) {
//        System.out.println(">>> " + node);
        if (node instanceof ValueNode) {
            return NodeType.MAP;
        }
        return super.type(node);
    }
    
    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        if (node instanceof ValueNode valueNode) {
            
        }
        
        // TODO Auto-generated method stub
        return super.entryStream(node);
    }
}
