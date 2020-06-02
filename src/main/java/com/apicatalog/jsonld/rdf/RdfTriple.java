package com.apicatalog.jsonld.rdf;

/**
 * The {@link RdfTriple} interface describes an triple.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#webidl-1589662450">RdfTriple
 *      IDL</a>
 *
 */
public interface RdfTriple {

    /**
     * An absolute IRI or blank node identifier denoting the subject of the triple.
     * 
     * @return
     */
    String getSubject();

    String getPredicate();
    
    RdfObject getObject();
    
}