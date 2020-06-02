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
            if (scanner.accept(NQuadsTokenType.EOL, NQuadsTokenType.WS)) {
                continue;
            }
            
            dataset.add(reaStatement());
        }

        return dataset;
    }
    
    public RdfNQuad reaStatement() throws IOException, NQuadsReaderError {
        
        String subject = readSubject();
        
        String predicate = readPredicate();
        
        RdfObject object = readObject();
        
        if (!scanner.accept(NQuadsTokenType.STATEMENT_END)) {
            //TODO read graph name
        }
        
        scanner.expect(NQuadsTokenType.STATEMENT_END);
        
        return Rdf.createNQuad(null, predicate, object, null);
    }
    
    public String readSubject()  throws IOException, NQuadsReaderError {
        
        if (scanner.accept(NQuadsTokenType.IRIREF)) {
            return scanner.next();
        } 
        
        if (scanner.accept(NQuadsTokenType.BLANK_NODE_LABEL)) {
            return scanner.next();            
        }
        
        throw new NQuadsReaderError();
    }
    
    public String readPredicate()  throws IOException, NQuadsReaderError {        
        scanner.expect(NQuadsTokenType.IRIREF);
        return scanner.next();
    }
    
    public RdfObject readObject()  throws IOException, NQuadsReaderError {
        if (scanner.accept(NQuadsTokenType.IRIREF)) {
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
}
