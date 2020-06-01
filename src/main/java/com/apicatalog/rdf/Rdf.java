package com.apicatalog.rdf;

import java.io.Reader;
import java.io.Writer;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.rdf.io.RdfParser;
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
//    public static final Graph createGraph() {
//        return RdfProvider.provider().createGraph();
//    }

    public static final RdfParser createParser(Reader reader) {
        return RdfProvider.provider().createParser(reader);
    }

    public static final RdfWriter createWriter(Writer writer) {
        return RdfProvider.provider().createWriter(writer);
    }

    public static final Dataset createDataset() {
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
