package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.impl.DefaultRdfProvider;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedContentException;

public abstract class RdfProvider {

    private static RdfProvider provider = null;
    
    protected RdfProvider() {   
    }
    
    public static final RdfProvider provider() {
        return provider != null ? provider : DefaultRdfProvider.INSTANCE;
    }
    
    public static final void setProvider(RdfProvider instance) {
        provider = instance;
    }

    public abstract RdfDataset createDataset();

    public abstract Collection<MediaType> canRead();
    
    public abstract RdfReader createReader(MediaType contentType, Reader reader) throws UnsupportedContentException;

    public abstract Collection<MediaType> canWrite();
    
    public abstract RdfWriter createWriter(MediaType contentType, Writer writer) throws UnsupportedContentException;

    public abstract RdfGraph createGraph();
    
    public abstract RdfTriple createTriple(RdfResource subject, RdfResource predicate, RdfValue object);
    
    public abstract RdfNQuad createNQuad(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName);
    
    public abstract RdfResource createBlankNode(String value);
    
    public abstract RdfResource createIRI(String value);
        
    public abstract RdfLiteral createLangString(String lexicalForm, String langTag);
    
    public abstract RdfLiteral createTypedString(String lexicalForm, String datatype);   
}