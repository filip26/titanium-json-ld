/*
 * Copyright 2020 the original author or authors.
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
package com.apicatalog.jsonld.context;

import java.util.Collection;
import java.util.Optional;

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
    
    public Optional<String> match(final Collection<String> preferredValues) {

        // 1. If the active context has a null inverse context, 
        //    set inverse context in active context to the result of calling 
        //    the Inverse Context Creation algorithm using active context.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        // 2. Initialize inverse context to the value of inverse context in active context.
        final InverseContext inverseContext = activeContext.getInverseContext();
 
        // 4. For each item container in containers:
        for (final String container : containers) {
 
            if (inverseContext.doesNotContain(variable, container, typeLanguage)) {
                continue;
            }
                // 4.4.
            for (final String item : preferredValues) {
                
                // 4.4.1.
                if (inverseContext.contains(variable, container, typeLanguage, item)) {
                    return inverseContext.get(variable, container, typeLanguage, item);
                }
            }
        }   

        // 5.
        return Optional.empty();
    } 
}