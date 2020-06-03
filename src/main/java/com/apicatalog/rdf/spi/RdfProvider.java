package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfFormat;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.impl.JsonLdRdfProvider;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;

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

    public abstract RdfTriple createTriple(IRI subject, IRI predicate, IRI object);
    
    public abstract RdfTriple createTriple(IRI subject, IRI predicate, RdfObject object);
    
    public abstract RdfTriple createTriple(RdfSubject subject, IRI predicate, RdfObject object);
    
    public abstract RdfNQuad createNQuad(RdfSubject subject, IRI predicate, RdfObject object, String graphName);

    public abstract RdfSubject createSubject(IRI iri);
    
    public abstract RdfSubject createSubject(BlankNode blankNode);

    public abstract RdfLiteral createLiteral(String lexicalForm);

    public abstract RdfLiteral createLiteral(String lexicalForm, IRI dataType);
    
    public abstract RdfLiteral createLiteral(String lexicalForm, String langTag);

    public abstract RdfObject createObject(IRI iri);

    public abstract RdfObject createObject(RdfLiteral literal);

    public abstract RdfObject createObject(BlankNode blankNode);
}
