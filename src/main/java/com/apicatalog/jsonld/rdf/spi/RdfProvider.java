package com.apicatalog.jsonld.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfFormat;
import com.apicatalog.jsonld.rdf.RdfGraph;
import com.apicatalog.jsonld.rdf.RdfLiteral;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfTriple;
import com.apicatalog.jsonld.rdf.impl.JsonLdRdfProvider;
import com.apicatalog.jsonld.rdf.io.RdfReader;
import com.apicatalog.jsonld.rdf.io.RdfWriter;

public abstract class RdfProvider {

    protected RdfProvider() {
        
    }
    
    public static RdfProvider provider() {
        //TODO
        return JsonLdRdfProvider.INSTANCE;
    }

    public abstract RdfDataset createDataset();

    public abstract RdfReader createReader(Reader reader, RdfFormat format);

    public abstract RdfWriter createWriter(Writer writer, RdfFormat format);

    public abstract RdfGraph createGraph();

    public abstract RdfTriple createTriple(String subject, String predicate, String object);
    
    public abstract RdfTriple createTriple(String subject, String predicate, RdfLiteral object);

    public abstract RdfTriple createTriple(String subject, String predicate, RdfObject object);
    
}
