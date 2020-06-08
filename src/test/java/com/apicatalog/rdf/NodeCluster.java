package com.apicatalog.rdf;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

final class NodeCluster {

    Map<String, Integer> source;
    
    String[] target;
    
    int[] indices;
    int[] mapping;
    
    int index;

    int permutations;
    
    public NodeCluster(Map<String, Integer> source, String[] target, int permutations) {
        this.source = source;
        this.target = target;
        
        this.indices = new int[source.size()];
        this.mapping = new int[source.size()];
        
        this.permutations = permutations;
        
        reset();
    }
    
    public String[] target() {
        return target;
    }
    
    public static final NodeCluster create(Collection<String> source, Collection<String> target) {
        
        if (source.size() != target.size()) {
            throw new IllegalArgumentException();
        }
        
        Map<String, Integer> sourceMap = new HashMap<>();
        for (String s : source) {
            sourceMap.put(s, sourceMap.size());
        }

        return new NodeCluster(sourceMap, target.toArray(new String[0]), factorial(source.size()));
    }
    
    private static final int factorial(int number) {
        
        int result = 1;
        
        for(int i=1; i <= number; i++){    
            result=result*i;    
        }   
        return result;
    }
    
    public boolean next() {
        
        if (indices.length == 1) {
            return true;
        }
        
        while (index < indices.length) {
            if (indices[index] < index) {
                swap(index % 2 == 0 ?  0 : indices[index], index);
                indices[index]++;
                index = 0;
                break;
            }
            else {
                indices[index] = 0;
                index++;
            }
        }
        
        if (index >= indices.length) {
            reset(); 
            return true;
        }
        
        return false;
    }
    
    private void swap(int a, int b) {
        int tmp = mapping[a];
        mapping[a] = mapping[b];
        mapping[b] = tmp;
    }
    
    private void reset() {
        for (int i=0; i < indices.length; i++) {
            indices[i] = 0;
            mapping[i] = i;
        }
        index = 0;
    }
    
    public String mapping(String label) {
        return target[mapping[source.get(label)]];
    }
    
    public int permutations() {
        return permutations;
    }    
}
