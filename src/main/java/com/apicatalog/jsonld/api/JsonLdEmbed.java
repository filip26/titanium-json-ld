package com.apicatalog.jsonld.api;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-framing/#dom-jsonldembed">JsonLdEmbed</a>
 *
 */
public enum JsonLdEmbed {

    /**
     * Always embed node objects as property values, 
     * unless this would cause a circular reference.
     */
    ALWAYS,
    
    /**
     * Always use a node reference when serializing matching values.
     */
    NEVER,
    
    /**
     * Only a single value within a given node object should be embedded,
     * other values of other properties use a node reference. 
     * This is the default value if neither @embed nor object embed flag is specified.
     */
    ONCE
    
}
