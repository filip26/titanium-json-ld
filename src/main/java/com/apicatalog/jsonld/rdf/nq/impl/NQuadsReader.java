package com.apicatalog.jsonld.rdf.nq.impl;

import java.io.IOException;
import java.io.Reader;

import com.apicatalog.jsonld.rdf.Rdf;
import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfNQuad;
import com.apicatalog.jsonld.rdf.RdfObject;
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
        
        String subject = readSubject();
        
        skipWs();
        
        String predicate = readPredicate();

        skipWs();
        
        RdfObject object = readObject();
        
        skipWs();
        
        if (!scanner.accept(NQuadsTokenType.END_OF_STATEMENT)) {
            //TODO read graph name
        }

        scanner.expect(NQuadsTokenType.END_OF_STATEMENT);

        return Rdf.createNQuad(null, predicate, object, null);
    }
    
    public String readSubject()  throws IOException, NQuadsReaderError {
        
        if (scanner.accept(NQuadsTokenType.IRI_REF)) {
            return scanner.next();
        } 
        
        if (scanner.accept(NQuadsTokenType.BLANK_NODE_LABEL)) {
            return scanner.next();            
        }
        
        throw new NQuadsReaderError();
    }
    
    public String readPredicate()  throws IOException, NQuadsReaderError {        
        scanner.expect(NQuadsTokenType.IRI_REF);
        return scanner.next();
    }
    
    public RdfObject readObject()  throws IOException, NQuadsReaderError {
        if (scanner.accept(NQuadsTokenType.IRI_REF)) {
            return Rdf.createObject(scanner.next());
        } 
        
        if (scanner.accept(NQuadsTokenType.BLANK_NODE_LABEL)) {
            return Rdf.createObject(scanner.next());
        }
        
        return readLiteral();
    }
    
    public RdfObject readLiteral()  throws IOException, NQuadsReaderError {
        
        scanner.expect(NQuadsTokenType.STRING_LITERAL_QUOTE);
        
        String text = scanner.next();
        
        //TODO lang etc.
        
        return null;
    }
    
    private void skipWs() throws IOException, NQuadsReaderError {
        
        while (scanner.hasNext() && scanner.accept(NQuadsTokenType.WHITE_SPACE)) {
            scanner.next();
        }
        
    }
}
