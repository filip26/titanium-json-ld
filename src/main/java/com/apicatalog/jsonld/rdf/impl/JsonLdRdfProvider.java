package com.apicatalog.jsonld.rdf.impl;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfFormat;
import com.apicatalog.jsonld.rdf.RdfGraph;
import com.apicatalog.jsonld.rdf.RdfLiteral;
import com.apicatalog.jsonld.rdf.RdfNQuad;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfSubject;
import com.apicatalog.jsonld.rdf.RdfTriple;
import com.apicatalog.jsonld.rdf.io.RdfReader;
import com.apicatalog.jsonld.rdf.io.RdfWriter;
import com.apicatalog.jsonld.rdf.nq.impl.NQuadsReader;
import com.apicatalog.jsonld.rdf.nq.impl.NQuadsWriter;
import com.apicatalog.jsonld.rdf.spi.RdfProvider;

public final class JsonLdRdfProvider extends RdfProvider {

    public static final RdfProvider INSTANCE = new JsonLdRdfProvider(); 
    
    @Override
    public RdfDataset createDataset() {
        return new RdfDatasetImpl();
    }

    @Override
    public RdfReader createReader(Reader reader, RdfFormat format) {
        
        if (RdfFormat.NQuads.equals(format)) {
            return new NQuadsReader(reader);            
        }
        //TODO
        return null;
    }

    @Override
    public RdfWriter createWriter(Writer writer, RdfFormat format) {

        if (RdfFormat.NQuads.equals(format)) {
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
    public RdfTriple createTriple(IRI subject, IRI predicate, IRI object) {
        return RdfTripleImpl.create(subject, predicate, object);
    }

    @Override
    public RdfTriple createTriple(IRI subject, IRI predicate, RdfObject object) {
        return RdfTripleImpl.create(subject, predicate, object);
    }

    @Override
    public RdfTriple createTriple(RdfSubject subject, IRI predicate, RdfObject object) {
        return RdfTripleImpl.create(subject, predicate, object);
    }

    @Override
    public RdfNQuad createNQuad(RdfSubject subject, IRI predicate, RdfObject object, String graphName) {
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
    public RdfLiteral createLiteral(String lexicalForm) {
        
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }
        
        return new RdfLiteralImpl(lexicalForm);
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
    public RdfLiteral createLiteral(String lexicalForm, IRI dataType) {
        return new RdfLiteralImpl(lexicalForm, dataType);
    }

    @Override
    public RdfLiteral createLiteral(String lexicalForm, String langTag) {
        return new RdfLiteralImpl(lexicalForm, langTag);
    }
    
}
