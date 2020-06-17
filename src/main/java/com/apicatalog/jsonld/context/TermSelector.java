package com.apicatalog.jsonld.context;

import java.util.Collection;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#term-selection">Term Selection</a>
 *
 */
public final class TermSelector {

    // required
    private final ActiveContext activeContext;
    
    private final String variable;
    
    private final Collection<String> containers;
    
    private final String typeLanguage;
    
    private TermSelector(final ActiveContext activeContext, final String variable, final Collection<String> containers, final String typeLanguage) {
        this.activeContext = activeContext;
        this.variable = variable;
        this.containers = containers;
        this.typeLanguage = typeLanguage;
    }
    
    public static TermSelector with(final ActiveContext activeContext, final String variable, final Collection<String> containers, final String typeLanguage) {
        return new TermSelector(activeContext, variable, containers, typeLanguage);
    }
    
    public String select(final Collection<String> preferredValues) {

        // 1. If the active context has a null inverse context, 
        //    set inverse context in active context to the result of calling 
        //    the Inverse Context Creation algorithm using active context.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        // 2. Initialize inverse context to the value of inverse context in active context.
        final InverseContext inverseContext = activeContext.getInverseContext();
               
        // 4. For each item container in containers:
        for (String container : containers) {
 
            if (inverseContext.doesNotContain(variable, container, typeLanguage)) {
                continue;
            }
                // 4.4.
            for (final String item : preferredValues) {
                
                // 4.4.1.
                if (inverseContext.doesNotContain(variable, container, typeLanguage, item)) {
                    continue;
                }

                return inverseContext.get(variable, container, typeLanguage, item);
            }
        }   

        // 5.
        return null;
    } 
}