package com.apicatalog.jsonld.rdf;

import java.util.List;
import java.util.Map;

final class RdfBlankNodeMapper {

//    private final Map<String, MapGroup> set1;
//    private final Map<String, MapGroup> set2;
//    
    private RdfBlankNodeMapper() {
//        this.set1 = new HashMap<>();
//        this.set2 = new HashMap<>();
    }
    
    protected static RdfBlankNodeMapper create(List<RdfTriple> from, List<RdfTriple> to) {
        // TODO Auto-generated method stub
        return new RdfBlankNodeMapper();
    }

    public int mappings() {
        return 1;
    }


    public Map<String, String> mapping(int number) {
        return null;
    }
    
    static final class MapGroup {

        Map<String, String> set1;
        Map<String, String> set2;
        
        
        String[] id1;
        String[] id2;
        
        
        
    }

    
    
}
