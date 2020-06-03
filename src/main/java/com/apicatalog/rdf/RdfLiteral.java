package com.apicatalog.rdf;

import java.util.Optional;

import com.apicatalog.iri.IRI;

/**
 * The {@link RdfLiteral} interface describes an <code>RDF Literal</code>.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#webidl-2088240233">RdfLiteral
 *      IDL</a>
 *
 */
public interface RdfLiteral {

    /**
     * Get the lexical value of the literal
     * 
     * @return lexical value
     */
    String getValue();

    /**
     * An absolute IRI denoting the datatype IRI of the literal. If the value is
     * rdf:langString, language MUST be specified.
     * 
     * @return
     */
    IRI getDatatype();

    /**
     * An optional language tag as defined by [BCP47]. If this value is specified,
     * datatype MUST be rdf:langString.
     * 
     * @return
     */
    Optional<String> getLanguage();

}