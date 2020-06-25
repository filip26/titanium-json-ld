package com.apicatalog.rdf;

import java.util.List;

import com.apicatalog.jsonld.JsonLd;


/**
 * The {@link RdfGraph} interface describes operations on an RDF graph used by
 * the @link {@link JsonLd#fromRdf(RdfDataset)} and
 * {@link JsonLd#toRdf(java.net.URI)} methods in the
 * {@link JsonLd} interface. The interface may be used for constructing
 * a new {@link RdfGraph}, which is composed of zero or more {@link RdfTriple}
 * instances.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-140206580">RdfGraph
 *      IDL</a>
 * 
 */
public interface RdfGraph {

    boolean contains(RdfTriple triple);
    
    List<RdfTriple> toList();

}
