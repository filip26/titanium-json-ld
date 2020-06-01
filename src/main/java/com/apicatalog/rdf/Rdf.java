package com.apicatalog.rdf;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class Rdf {

    private Rdf() {
    }
    
//    public static final BlankNode createBlankNode() {
//        return RdfProvider.provider().createBlankNode();
//    }
//
//    public static final BlankNode createBlankNode(String name) {
//        return RdfProvider.provider().createBlankNode(name);
//    }
//
    public static final RdfGraph createGraph() {
        return RdfProvider.provider().createGraph();
    }

    public static final RdfReader createReader(Reader reader, RdfFormat format) {
        return RdfProvider.provider().createReader(reader, format);
    }

    public static final RdfWriter createWriter(Writer writer, RdfFormat format) {
        return RdfProvider.provider().createWriter(writer, format);
    }

    public static final RdfDataset createDataset() {
        return RdfProvider.provider().createDataset();
    }
//TODO
//    public static final Literal createLiteral(String lexicalForm) throws IllegalArgumentException {
//        return RdfProvider.provider().createLiteral(lexicalForm);
//    }
//
//
//    public static final Literal createLiteral(String lexicalForm, IRI dataType) throws IllegalArgumentException {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    public static final Literal createLiteral(String lexicalForm, String languageTag) throws IllegalArgumentException {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    public static final Triple createTriple(BlankNodeOrIRI subject, IRI predicate, RDFTerm object) throws IllegalArgumentException {
//        // TODO Auto-generated method stub
//        return null;
//    }
}
