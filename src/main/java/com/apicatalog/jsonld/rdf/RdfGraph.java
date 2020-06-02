package com.apicatalog.jsonld.rdf;

import java.util.List;
import java.util.stream.Stream;

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

    /**
     * Adds an RdfTriple to the RdfGraph. Used by the Deserialize JSON-LD to RDF
     * Algorithm.
     * 
     * @param triple The RdfTriple to add to the RdfGraph
     */
    void add(RdfTriple triple);

    Stream<? extends RdfTriple> stream();

    List<? extends RdfTriple> getList();

}
