package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.impl.DefaultRdfProvider;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;

public abstract class RdfProvider {

    protected RdfProvider() {   
    }
    
    public static RdfProvider provider() {
        return DefaultRdfProvider.INSTANCE;
    }

    public abstract RdfDataset createDataset();

    public abstract RdfReader createReader(Reader reader, RdfFormat format) throws UnsupportedFormatException;

    public abstract RdfWriter createWriter(Writer writer, RdfFormat format) throws UnsupportedFormatException;

    public abstract RdfGraph createGraph();
    
    public abstract RdfTriple createTriple(RdfResource subject, RdfResource predicate, RdfValue object);
    
    public abstract RdfNQuad createNQuad(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName);
    
    public abstract RdfResource createBlankNode(String value);
    
    public abstract RdfResource createIRI(String value);
        
    public abstract RdfLiteral createLangString(String lexicalForm, String langTag);
    
    public abstract RdfLiteral createTypedString(String lexicalForm, String datatype);   
}