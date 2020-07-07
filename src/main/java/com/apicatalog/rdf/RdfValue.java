package com.apicatalog.rdf;

/**
 * 
 * Represents an immutable RDF statement's value.
 * 
 * @since 0.8.4
 */
public interface RdfValue {

    /**
     * Indicates if the value type is {@link ValueType#LITERAL}.
     *
     * @return <code>true</code> if the value type is literal, <code>false</code> otherwise. 
     */
    default boolean isLiteral() {
        return false;
    }

    /**
     * Indicates if the value type is {@link ValueType#IRI}.
     *
     * @return <code>true</code> if the value type is IRI, <code>false</code> otherwise. 
     */
    default boolean isIRI() {
        return false;
    }

    /**
     * Indicates if the value type is {@link ValueType#BLANK_NODE}.
     *
     * @return <code>true</code> if the value type is blank node, <code>false</code> otherwise. 
     */
    default boolean isBlankNode() {
        return false;
    }
    
    /**
     * Return the RdfValue as a RdfLiteral
     *
     * @return the RdfValue as a RdfLiteral
     * @throws ClassCastException if the RdfValue is not a RdfLiteral
     *
     */
    RdfLiteral asLiteral();    
    
    /**
     * Returns raw {@link String} representation of the value.
     * 
     * @return  text representing the value.
     */
    String getValue();

    
    @Override
    boolean equals(Object o);

    @Override
    int hashCode();
    
    @Override
    String toString();
}
