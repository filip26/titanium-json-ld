package com.apicatalog.jsonld.rdf;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

final class NodeCluster {

    Map<String, Integer> source;
    
    String[] target;
    
    int[] index;
    int[] mapping;
    
    int iteration;
    int permutations;
    
    public NodeCluster(Map<String, Integer> source, String[] target, int permutations) {
        this.source = source;
        this.target = target;
        
        this.index = new int[source.size()];
        this.mapping = new int[source.size()];
        
        this.permutations = permutations;
        
        reset();
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
        
        boolean overflow = iteration >= index.length; 
        
        if (overflow) {
            reset();
        }
                
        if (iteration < index.length) {
            if (index[iteration] < iteration) {
                swap(iteration % 2 == 0 ?  0: index[iteration], iteration);
                index[iteration]++;
                iteration = 0;
            }
            else {
                index[iteration] = 0;
                iteration++;
            }
        }
        
        return overflow;
    }
    
    private void swap(int a, int b) {
        int tmp = mapping[a];
        mapping[a] = mapping[b];
        mapping[b] = tmp;
    }
    
    private void reset() {
        for (int i=0; i < index.length; i++) {
            index[i] = 0;
            mapping[i] = i;
        }
        iteration = 0;
    }
    
    public String mapping(String label) {
        return target[mapping[source.get(label)]];
    }
    
    public int permutations() {
        return permutations;
    }
    
}
