package com.apicatalog.rdf;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.spi.RdfProvider;

public final class Rdf {

    private Rdf() {
    }
    
    public static final RdfGraph createGraph() {
        return RdfProvider.provider().createGraph();
    }

    public static final RdfReader createReader(Reader reader, RdfFormat format) throws UnsupportedFormatException {

        if (reader == null || format == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createReader(reader, format);
    }
    
    public static final RdfReader createReader(InputStream is, RdfFormat format) throws UnsupportedFormatException {
        
        if (is == null || format == null) {
            throw new IllegalArgumentException();
        }

        return createReader(new InputStreamReader(is), format);
    }

    public static final RdfWriter createWriter(Writer writer, RdfFormat format) throws UnsupportedFormatException {
        
        if (writer == null || format == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createWriter(writer, format);
    }

    public static final RdfWriter createWriter(OutputStream os, RdfFormat format) throws UnsupportedFormatException {
        
        if (os == null || format == null) {
            throw new IllegalArgumentException();
        }

        return createWriter(new OutputStreamWriter(os), format);
    }

    public static final RdfDataset createDataset() {
        return RdfProvider.provider().createDataset();
    }

    public static final RdfTriple createTriple(RdfSubject subject, RdfPredicate predicate, RdfObject object) {
        
        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createTriple(subject, predicate, object);
    }

    public static final RdfNQuad createNQuad(RdfSubject subject, RdfPredicate predicate, RdfObject object, RdfGraphName graphName) {
        
        if (subject == null) {            
            throw new IllegalArgumentException("Subject cannot be null.");
        }
        if (predicate == null) {
            throw new IllegalArgumentException("Predicate cannot be null.");
        }
        if (object == null) {
            throw new IllegalArgumentException("Object cannot be null.");            
        }

        return RdfProvider.provider().createNQuad(subject, predicate, object, graphName);
    }

    /**
     * Create a new {@link RdfSubject}.
     * 
     * @param subject an absolute IRI or blank node identifier
     * @return {@link RdfSubject}
     */
    public static RdfSubject createSubject(final String subject) {

        if (subject == null) {            
            throw new IllegalArgumentException("The subject cannot be null.");
        }
        
        if (UriUtils.isAbsoluteUri(subject)) {
            return createSubject(RdfSubject.Type.IRI, subject);
        }
        
        if (BlankNode.isWellFormed(subject)) {
            return createSubject(RdfSubject.Type.BLANK_NODE, subject);
        }
        
        throw new IllegalArgumentException("The subject must be an absolute IRI or blank node identifier, but was [" + subject + "].");
    }
    
    public static RdfSubject createSubject(RdfSubject.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createSubject(type, value);        
    }

    /**
     * Create a new {@link RdfPredicate}.
     * 
     * @param predicate an absolute IRI or blank node identifier
     * @return {@link RdfPredicate}
     */
    public static RdfPredicate createPredicate(String predicate) {
        
        if (predicate == null) {            
            throw new IllegalArgumentException("The predicate cannot be null.");
        }
        
        if (UriUtils.isAbsoluteUri(predicate)) {
            return createPredicate(RdfPredicate.Type.IRI, predicate);
        }

        if (BlankNode.isWellFormed(predicate)) {
            return createPredicate(RdfPredicate.Type.BLANK_NODE, predicate);
        }

        throw new IllegalArgumentException("The predicate must be an absolute IRI or blank node identifier, but was [" + predicate + "].");
    }
    
    public static RdfPredicate createPredicate(RdfPredicate.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createPredicate(type, value);
    }

    public static RdfObject createObject(RdfObject.Type type, String value) {
        
        if (type == null || value == null /*|| value.isBlank()*/) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createObject(type, value);
    }
    
    public static RdfObject createObject(RdfLiteral literal) {
        
        if (literal == null) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createObject(literal);
    }
    
    public static RdfLiteral createTypedString(String lexicalForm, String dataType) {
        
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createLiteral(lexicalForm, null, dataType);
    }
    
    public static RdfLiteral createLangString(String lexicalForm, String langTag) {
        
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createLiteral(lexicalForm, langTag, RdfConstants.LANG_STRING);
    }

    /**
     * Create a new {@link RdfGraphName}.
     * 
     * @param graphName an absolute IRI or blank node identifier
     * @return {@link RdfGraphName}
     */
    public static RdfGraphName createGraphName(String graphName) {
        
        if (graphName == null) {            
            throw new IllegalArgumentException("The graph name cannot be null.");
        }
        
        if (UriUtils.isAbsoluteUri(graphName)) {
            return createGraphName(RdfGraphName.Type.IRI, graphName);
        }
        
        if (BlankNode.isWellFormed(graphName)) {
            return createGraphName(RdfGraphName.Type.BLANK_NODE, graphName);
        }
        
        throw new IllegalArgumentException("The graph name must be an absolute IRI or blank node identifier, but was [" + graphName + "].");        
    }
    
    public static RdfGraphName createGraphName(RdfGraphName.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createGraphName(type, value);
    }
}