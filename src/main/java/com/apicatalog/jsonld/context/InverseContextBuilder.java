package com.apicatalog.jsonld.context;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#inverse-context-creation">Inverse Context Creation</a>
 *
 */
public final class InverseContextBuilder {

    private final ActiveContext activeContext;
    
    private InverseContextBuilder(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }
    
    public static final InverseContextBuilder with(final ActiveContext activeContext) {
        return new InverseContextBuilder(activeContext);  
    }
    
    public InverseContext build() {
        
        //TODO
        return new InverseContext();
    }
    
}
