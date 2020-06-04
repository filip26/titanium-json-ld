package com.apicatalog.rdf;

import com.apicatalog.jsonld.api.JsonLdProcessor;

/**
 * The {@link RdfGraph} interface describes operations on an RDF graph used by
 * the {@link JsonLdProcessor#fromRdf() fromRdf()} and
 * {@link JsonLdProcessor#toRdf() toRdf()} methods in the
 * {@link JsonLdProcessor} interface. The interface may be used for constructing
 * a new {@link RdfGraph}, which is composed of zero or more {@link RdfTriple}
 * instances.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-140206580">RdfGraph
 *      IDL</a>
 * 
 */
public interface RdfGraph {

    boolean contains(RdfTriple triple);
        
}
