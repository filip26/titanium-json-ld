package com.apicatalog.jsonld.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public final class Utils {

    private Utils() {}
    
    public static final Collection<String> index(final Collection<String> keys, final boolean ordered) {

        if (ordered) {
            final List<String> sorted = new ArrayList<>(keys);
            
            Collections.sort(sorted);
            
            return sorted;
        }
        
        return keys;
    }
}