package com.apicatalog.rdf.impl;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfSubject.Type;
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
    public RdfTriple createTriple(RdfSubject subject, RdfPredicate predicate, RdfObject object) {
        return RdfTripleImpl.create(subject, predicate, object);
    }

    @Override
    public RdfNQuad createNQuad(RdfSubject subject, RdfPredicate predicate, RdfObject object, RdfGraphName graphName) {
        return new RdfNQuadImpl(subject, predicate, object, graphName);
    }

    @Override
    public RdfSubject createSubject(Type type, String value) {
        return new RdfSubjectImpl(type, value);
    }

    @Override
    public RdfPredicate createPredicate(RdfPredicate.Type type, String value) {
        return new RdfPredicateImpl(type, value);
    }

    @Override
    public RdfObject createObject(RdfObject.Type type, String value) {
        return new RdfObjectImpl(type, value);
    }

    @Override
    public RdfLiteral createLiteral(String lexicalForm, String langTag, String dataType) {
        return new RdfLiteralImpl(lexicalForm, langTag, dataType);
    }

    @Override
    public RdfGraphName createGraphName(RdfGraphName.Type type, String value) {
        return new RdfGraphNameImpl(type, value);
    }

    @Override
    public RdfObject createObject(RdfLiteral literal) {
        return new RdfObjectImpl(literal);
    }
}
