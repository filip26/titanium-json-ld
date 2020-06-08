package com.apicatalog.rdf.spi;

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
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.impl.DefaultRdfProvider;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;

public abstract class RdfProvider {

    protected RdfProvider() {
        
    }

    public static RdfProvider provider() {
        //TODO
        return DefaultRdfProvider.INSTANCE;
    }

    public abstract RdfDataset createDataset();

    public abstract RdfReader createReader(Reader reader, RdfFormat format);

    public abstract RdfWriter createWriter(Writer writer, RdfFormat format);

    public abstract RdfGraph createGraph();
    
    public abstract RdfTriple createTriple(RdfSubject subject, RdfPredicate predicate, RdfObject object);
    
    public abstract RdfNQuad createNQuad(RdfSubject subject, RdfPredicate predicate, RdfObject object, RdfGraphName graphName);

    public abstract RdfSubject createSubject(RdfSubject.Type type, String value);
    
    public abstract RdfPredicate createPredicate(RdfPredicate.Type type, String value);

    public abstract RdfObject createObject(RdfObject.Type type, String value);
    
    public abstract RdfObject createObject(RdfLiteral literal);
    
    public abstract RdfLiteral createTypedString(String lexicalForm, String dataType);
    
    public abstract RdfLiteral createLangString(String lexicalForm, String langTag);

    public abstract RdfGraphName createGraphName(RdfGraphName.Type type, String value);

}
