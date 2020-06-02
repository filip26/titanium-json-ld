package com.apicatalog.jsonld.rdf.nq.impl;

import java.io.IOException;
import java.io.Reader;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.rdf.Rdf;
import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfNQuad;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfSubject;
import com.apicatalog.jsonld.rdf.io.RdfReader;

public final class NQuadsReader implements RdfReader {

    private final NQuadsScanner scanner;
    
    public NQuadsReader(final Reader reader) {
        this.scanner = new NQuadsScanner(reader);
    }
    
    @Override
    public RdfDataset readDataset() throws IOException, NQuadsReaderError {

        //TODO cache
        
        RdfDataset dataset = Rdf.createDataset();

        while (scanner.hasNext()) {

            // skip EOL and whitespace
            if (scanner.accept(NQuadsTokenType.END_OF_LINE, NQuadsTokenType.WHITE_SPACE)) {
                continue;
            }
            
            dataset.add(reaStatement());
        }

        return dataset;
    }
    
    public RdfNQuad reaStatement() throws IOException, NQuadsReaderError {
        
        RdfSubject subject = readSubject();
        
        skipWs();
        
        IRI predicate = readIri();

        skipWs();
        
        RdfObject object = readObject();
        
        skipWs();
        
        if (!scanner.accept(NQuadsTokenType.END_OF_STATEMENT)) {
            //TODO read graph name
        }

        scanner.expect(NQuadsTokenType.END_OF_STATEMENT);
        scanner.next();

        return Rdf.createNQuad(subject, predicate, object, null);
    }
    
    public RdfSubject readSubject()  throws IOException, NQuadsReaderError {
        
        if (scanner.accept(NQuadsTokenType.IRI_REF)) {
            return Rdf.createSubject(IRI.create(scanner.next()));
        } 
        
        if (scanner.accept(NQuadsTokenType.BLANK_NODE_LABEL)) {
            return Rdf.createSubject(BlankNode.create(scanner.next()));            
        }
        
        throw new NQuadsReaderError();
    }
    
    public IRI readIri()  throws IOException, NQuadsReaderError {        
        scanner.expect(NQuadsTokenType.IRI_REF);
        return IRI.create(scanner.next());
    }
    
    public RdfObject readObject()  throws IOException, NQuadsReaderError {
        if (scanner.accept(NQuadsTokenType.IRI_REF)) {
            return Rdf.createObject(IRI.create(scanner.next()));
        } 
        
        if (scanner.accept(NQuadsTokenType.BLANK_NODE_LABEL)) {
            return Rdf.createObject(BlankNode.create(scanner.next()));
        }
        
        return readLiteral();
    }
    
    public RdfObject readLiteral()  throws IOException, NQuadsReaderError {
        
        scanner.expect(NQuadsTokenType.STRING_LITERAL_QUOTE);
        
        String text = scanner.next();
        
        //TODO lang etc.
        
        return Rdf.createObject(Rdf.createLitteral(text));
    }
    
    private void skipWs() throws IOException, NQuadsReaderError {
        
        while (scanner.hasNext() && scanner.accept(NQuadsTokenType.WHITE_SPACE)) {
            scanner.next();
        }
        
    }
}
