package com.apicatalog.rdf;

import org.apache.commons.rdf.api.BlankNode;
import org.apache.commons.rdf.api.BlankNodeOrIRI;
import org.apache.commons.rdf.api.Dataset;
import org.apache.commons.rdf.api.Graph;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.api.Literal;
import org.apache.commons.rdf.api.Quad;
import org.apache.commons.rdf.api.RDFTerm;
import org.apache.commons.rdf.api.Triple;

public final class Rdf {

    private Rdf() {
        
    }
    
    public static final BlankNode createBlankNode() {
        // TODO Auto-generated method stub
        return null;
    }

    public static final BlankNode createBlankNode(String name) {
        // TODO Auto-generated method stub
        return null;
    }

    public static final Graph createGraph() {
        // TODO Auto-generated method stub
        return null;
    }

    public static final Dataset createDataset() {
        // TODO Auto-generated method stub
        return null;
    }

    public static final IRI createIRI(String iri) throws IllegalArgumentException {
        // TODO Auto-generated method stub
        return null;
    }

    public static final Literal createLiteral(String lexicalForm) throws IllegalArgumentException {
        // TODO Auto-generated method stub
        return null;
    }


    public static final Literal createLiteral(String lexicalForm, IRI dataType) throws IllegalArgumentException {
        // TODO Auto-generated method stub
        return null;
    }

    public static final Literal createLiteral(String lexicalForm, String languageTag) throws IllegalArgumentException {
        // TODO Auto-generated method stub
        return null;
    }

    public static final Triple createTriple(BlankNodeOrIRI subject, IRI predicate, RDFTerm object) throws IllegalArgumentException {
        // TODO Auto-generated method stub
        return null;
    }

    public static final Quad createQuad(BlankNodeOrIRI graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object)
            throws IllegalArgumentException {
        // TODO Auto-generated method stub
        return null;
    }
}
