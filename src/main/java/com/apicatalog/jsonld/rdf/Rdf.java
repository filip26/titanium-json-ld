package com.apicatalog.jsonld.rdf;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.rdf.io.RdfReader;
import com.apicatalog.jsonld.rdf.io.RdfWriter;
import com.apicatalog.jsonld.rdf.spi.RdfProvider;

public final class Rdf {

    private Rdf() {
    }
    
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

    public static final RdfTriple createTriple(IRI subject, IRI predicate, IRI object) {
        return RdfProvider.provider().createTriple(subject, predicate, object);
    }
    
    public static final RdfTriple createTriple(IRI subject, IRI predicate, RdfObject object) {
        return RdfProvider.provider().createTriple(subject, predicate, object);
    }

    public static final RdfNQuad createNQuad(RdfSubject object, IRI predicate, RdfObject subject, String graphName) {
        return RdfProvider.provider().createNQuad(object, predicate, subject, graphName);
    }

    public static final RdfObject createObject(IRI iri) {
        return RdfProvider.provider().createObject(iri);
    }

    public static final RdfObject createObject(RdfLiteral literal) {
        return RdfProvider.provider().createObject(literal);
    }

    public static final RdfWriter createWriter(OutputStream os, RdfFormat format) {
        return RdfProvider.provider().createWriter(new OutputStreamWriter(os), format);
    }

    public static final RdfSubject createSubject(IRI iri) {
        return RdfProvider.provider().createSubject(iri);
    }

    public static final  RdfSubject createSubject(BlankNode blankNode) {
        return RdfProvider.provider().createSubject(blankNode);
    }

    public static final RdfLiteral createLitteral(String lexicalForm) {
        return RdfProvider.provider().createLiteral(lexicalForm);
    }

    public static final RdfLiteral createLitteral(String lexicalForm, IRI dataType) {
        return RdfProvider.provider().createLiteral(lexicalForm, dataType);
    }
    
    public static final RdfLiteral createLitteral(String lexicalForm, String langTag) {
        return RdfProvider.provider().createLiteral(lexicalForm, langTag);
    }


    public static final RdfObject createObject(BlankNode blankNode) {
        return RdfProvider.provider().createObject(blankNode);
    }

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
