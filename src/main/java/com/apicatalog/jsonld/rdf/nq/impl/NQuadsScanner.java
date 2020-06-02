package com.apicatalog.jsonld.rdf.nq.impl;

import java.io.BufferedReader;
import java.io.Reader;

/**
 * 
 * @see <a href="https://www.w3.org/TR/n-quads/#sec-grammar">N-Quads Grammar</a>
 *
 */
class NQuadsScanner {

    private final Reader reader;

    private String lastValue;
    private NQuadsTokenType lastType;
    
    protected NQuadsScanner(Reader reader) {
        this.reader = new BufferedReader(reader, 8192);
        
        this.lastType = null;
        this.lastValue = null;
    }
    
    public boolean hasNext() {
        return NQuadsTokenType.EOF.equals(lastType);
    }

    public boolean accept(NQuadsTokenType ...types) {
        
        if (!hasNext()) {
            return false;
        }
        readNext();
        
        // TODO Auto-generated method stub
        return false;
    }

    public void expect(NQuadsTokenType type) throws NQuadsReaderError {

        if (!hasNext()) {
            throw new NQuadsReaderError();
        }
        readNext();

        
    }

    public String next() throws NQuadsReaderError {
        
        if (!hasNext()) {
            throw new NQuadsReaderError();
        }
        
        lastType = null;
        
        return lastValue;
    }

    void readNext() {
        if (lastType != null) {
            return;
        }
        
        //TODO
    }
    
}
