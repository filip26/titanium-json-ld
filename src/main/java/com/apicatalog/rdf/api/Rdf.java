package com.apicatalog.rdf.api;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class Rdf {

    private Rdf() {
    }
    
    public static final RdfGraph createGraph() {
        return RdfProvider.provider().createGraph();
    }

    public static final RdfReader createReader(Reader reader, RdfFormat format) {
        return RdfProvider.provider().createReader(reader, format);
    }
    
    public static final RdfReader createReader(InputStream is, RdfFormat format) {
        return createReader(new InputStreamReader(is), format);
    }

    public static final RdfWriter createWriter(Writer writer, RdfFormat format) {
        return RdfProvider.provider().createWriter(writer, format);
    }

    public static final RdfWriter createWriter(OutputStream os, RdfFormat format) {
        return createWriter(new OutputStreamWriter(os), format);
    }

    public static final RdfDataset createDataset() {
        return RdfProvider.provider().createDataset();
    }

    public static final StatementBuilder createStatementBuilder() {
        return new StatementBuilder();
    }
    
    public static final RdfTriple createTriple(RdfSubject subject, IRI predicate, RdfObject object) {
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
}
