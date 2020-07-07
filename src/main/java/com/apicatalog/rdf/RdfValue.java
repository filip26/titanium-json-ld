package com.apicatalog.rdf;

/**
 * 
 * Represents an immutable RDF value.
 * 
 * @since 0.8.4
 */
public interface RdfValue {

    /**
     * The type of a {@link RdfValue}.
     */
    enum ValueType { IRI, BLANK_NODE, LITERAL }
    
    /**
     * Indicates if the value type is {@link ValueType#LITERAL}.
     *
     * @return <code>true</code> if the value type is literal, <code>false</code> otherwise. 
     */
    boolean isLiteral();

    /**
     * Indicates if the value type is {@link ValueType#IRI}.
     *
     * @return <code>true</code> if the value type is IRI, <code>false</code> otherwise. 
     */
    boolean isIRI();

    /**
     * Indicates if the value type is {@link ValueType#BLANK_NODE}.
     *
     * @return <code>true</code> if the value type is blank node, <code>false</code> otherwise. 
     */
    boolean isBlankNode();

    /**
     * Returns the value type of this RDF value.
     *
     * @return RDF value type
     */
    ValueType getValueType();
    
    /**
     * Return the RdfValue as a RdfResource
     *
     * @return the RdfValue as a RdfResource
     * @throws ClassCastException if the RdfValue is not a RdfResource
     *
     */
    RdfResource asResource();

    /**
     * Return the RdfValue as a RdfLiteral
     *
     * @return the RdfValue as a RdfLiteral
     * @throws ClassCastException if the RdfValue is not a RdfLiteral
     *
     */
    RdfLiteral asLiteral();    
    
    @Override
    boolean equals(Object o);

    @Override
    int hashCode();
}
