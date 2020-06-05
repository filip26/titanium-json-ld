package com.apicatalog.rdf.impl;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.nquad.NQuadsReader;
import com.apicatalog.rdf.io.nquad.NQuadsWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class DefaultRdfProvider extends RdfProvider {

    public static final RdfProvider INSTANCE = new DefaultRdfProvider(); 
    
    @Override
    public RdfDataset createDataset() {
        return new RdfDatasetImpl();
    }

    @Override
    public RdfReader createReader(Reader reader, RdfFormat format) {
        
        if (RdfFormat.N_QUADS.equals(format)) {
            return new NQuadsReader(reader);            
        }
        //TODO
        return null;
    }

    @Override
    public RdfWriter createWriter(Writer writer, RdfFormat format) {

        if (RdfFormat.N_QUADS.equals(format)) {
            return new NQuadsWriter(writer);            
        }
        //TODO
        return null;
    }

    @Override
    public RdfGraph createGraph() {
        return new RdfGraphImpl();
    }

    @Override
    public RdfTriple createTriple(RdfSubject subject, IRI predicate, RdfObject object) {
        return RdfTripleImpl.create(subject, predicate, object);
    }

    @Override
    public RdfNQuad createNQuad(RdfSubject subject, IRI predicate, RdfObject object, RdfGraphName graphName) {
        return new RdfNQuadImpl(subject, predicate, object, graphName);
    }

    @Override
    public RdfSubject createSubject(IRI iri) {
        return new RdfSubjectImpl(iri);
    }

    @Override
    public RdfSubject createSubject(BlankNode blankNode) {
        return new RdfSubjectImpl(blankNode);
    }

    @Override
    public RdfObject createObject(IRI iri) {
        return RdfObjectImpl.of(iri);
    }

    @Override
    public RdfObject createObject(RdfLiteral literal) {
        return RdfObjectImpl.of(literal);
    }

    @Override
    public RdfObject createObject(BlankNode blankNode) {
        return RdfObjectImpl.of(blankNode);
    }

    @Override
    public RdfLiteral createLiteral(String lexicalForm) {
        return new RdfLiteralImpl(lexicalForm);
    }

    @Override
    public RdfLiteral createLiteral(String lexicalForm, IRI dataType) {
        return new RdfLiteralImpl(lexicalForm, dataType);
    }

    @Override
    public RdfLiteral createLiteral(String lexicalForm, String langTag) {
        return new RdfLiteralImpl(lexicalForm, langTag);
    }

    @Override
    public RdfGraphName createGraphName(IRI graphName) {
        return new RdfGraphNameImpl(graphName);
    }

    @Override
    public RdfGraphName createGraphName(BlankNode graphName) {
        return new RdfGraphNameImpl(graphName);
    }
}
