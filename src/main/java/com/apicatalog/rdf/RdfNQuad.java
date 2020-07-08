package com.apicatalog.rdf;

import java.util.Optional;

/** 
 * Represents an immutable N-Quad statement. 
 */
public interface RdfNQuad extends RdfTriple {

    Optional<RdfResource> getGraphName();

}
