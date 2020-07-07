package com.apicatalog.rdf;

/** 
 * Represents an immutable N-Quad statement. 
 */
public interface RdfNQuad extends RdfTriple {

    RdfResource getGraphName();

}
