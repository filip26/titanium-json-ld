package com.apicatalog.rdf;

/**
 * The {@link RdfTriple} interface describes an immutable RDF triple. 
 */
public interface RdfTriple {

    /**
     * An absolute IRI or blank node identifier denoting the subject of the triple.
     * 
     * @return an absolute URI or blank node
     */
    RdfResource getSubject();

    /**
     * An absolute IRI or blank node identifier denoting the predicate of the triple.
     * 
     * @return an absolute URI or blank node
     */
    RdfResource getPredicate();


    /**
     * An absolute IRI or blank node identifier or {@link RdfLiteral}.
     * 
     * @return {@link RdfValue}
     */
    RdfValue getObject();

}
