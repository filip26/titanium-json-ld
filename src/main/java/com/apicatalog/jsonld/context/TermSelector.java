package com.apicatalog.jsonld.context;

import java.util.Collection;
import java.util.Map;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#term-selection">Term Selection</a>
 *
 */
public final class TermSelector {

    // mandatory
    private ActiveContext activeContext;
    
    private String variable;
    
    private Collection<String> containers;
    
    private String typeLanguage;
    
    private Collection<String> preferredValues;
    
    TermSelector(ActiveContext activeContext, String variable, Collection<String> containers, String typeLanguage, Collection<String> preferredValues) {
        this.activeContext = activeContext;
        this.variable = variable;
        this.containers = containers;
        this.typeLanguage = typeLanguage;
        this.preferredValues = preferredValues;
    }
    
    public static TermSelector with(ActiveContext activeContext, String variable, Collection<String> containers, String typeLanguage, Collection<String> preferredValues) {
        return new TermSelector(activeContext, variable, containers, typeLanguage, preferredValues);
    }
    
    public String select() {
        
        // 1. If the active context has a null inverse context, 
        //    set inverse context in active context to the result of calling 
        //    the Inverse Context Creation algorithm using active context.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        // 2. Initialize inverse context to the value of inverse context in active context.
        InverseContext inverseContext = activeContext.getInverseContext();
        
        // 3. Initialize container map to the value associated with var in the inverse context.
        Map<String, Map<String, Map<String, String>>> containerMap = inverseContext.getValue(variable); //TODO get rid of map in map in map ... 
        
        // 4. For each item container in containers:
        for (String container : containers) {
            
            // 4.1. If container is not an entry of container map, 
            //      then there is no term with a matching container mapping for it, 
            //      so continue to the next container.
            if (!containerMap.containsKey(container)) {
                continue;
            }
            
            // 4.2. Initialize type/language map to the value associated 
            //      with the container entry in container map.
            Map<String, Map<String, String>> typeLanguageMap = containerMap.get(container);
            
            // 4.3. Initialize value map to the value associated with 
            //      type/language entry in type/language map.
            Map<String, String> valueMap = typeLanguageMap.get(typeLanguage);
            
            // 4.4.
            for (final String item : preferredValues) {
                
                // 4.4.1.
                if (!valueMap.containsKey(item)) {
                    continue;
                }
                
                return valueMap.get(item);
            }
        }
        
        // 5.
        return null;
    }
    
    
}
