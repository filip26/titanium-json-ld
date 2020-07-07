package com.apicatalog.rdf;

import java.util.Optional;

/**
 * The {@link RdfLiteral} interface describes an immutable <code>RDF Literal</code>.
 */
public interface RdfLiteral extends RdfValue {

    /**
     * Get the lexical value of the literal.
     * 
     * @return lexical value, never <code>null</code>
     */
    @Override
    String getValue();

    /**
     * An absolute IRI denoting the datatype IRI of the literal. If the value is
     * rdf:langString, {@link #getLanguage()} value is present.
     * 
     * @return an absolute IRI, never <code>null</code>
     */
    String getDatatype();

    /**
     * An optional language tag. If this value is specified, {@link #getDatatype()} returns rdf:langString.
     * 
     * @return language tag or {@link Optional#empty()} if not set
     */
    Optional<String> getLanguage();    
    
    @Override
    default boolean isIRI() {
        return false;
    }
    
    @Override
    default boolean isBlankNode() {
        return false;
    }
    
    @Override
    default RdfLiteral asLiteral() {
        return this;
    }
}