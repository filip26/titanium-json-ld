package com.apicatalog.jsonld.rdf.nq.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;

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
    
    public boolean hasNext() throws NQuadsReaderError {
        readNext();
        return !NQuadsTokenType.END_OF_INPUT.equals(lastType);
    }

    public boolean accept(NQuadsTokenType ...types) throws NQuadsReaderError {
        
        if (!hasNext()) {
            return false;
        }
        
        return Arrays.asList(types).stream().anyMatch(lastType::equals);
    }

    public void expect(NQuadsTokenType ...types) throws NQuadsReaderError {

        if (!hasNext()) {
            throw new NQuadsReaderError();
        }

        if (Arrays.asList(types).stream().noneMatch(lastType::equals)) {
            throw new NQuadsReaderError();
        }
    }

    public String next() throws NQuadsReaderError {
        
        if (!hasNext()) {
            throw new NQuadsReaderError();
        }
        
        if (lastType == null) {
            readNext();
            return next();
        }
        
        lastType = null;
        
        return lastValue;
    }

    void readNext() throws NQuadsReaderError {
        
        if (lastType != null) {
            return;
        }
        
        try {
            int ch = reader.read();

            // EOF
            if (ch == -1) {
                lastValue = null;
                lastType = NQuadsTokenType.END_OF_INPUT;

            // WS
            } else if (ch == ' ' || ch == '\t') {
                
                readWhitespace();
                
            // EOL
            } else if (ch == '\r' || ch == '\n') {
                
                readEOL();

            } else if (ch == '@') {
                
            } else if (ch == '<') {
                
                readIriRef();
                
            } else if (ch == '"') {
                
                readString();

            } else if (ch == '.') {

                lastType = NQuadsTokenType.END_OF_STATEMENT;
                lastValue = null;
                
            } else {
                throw new NQuadsReaderError();
            }
                
//            System.out.println("Token " + lastType + ", " + lastValue);  


        } catch (IOException e) {
            e.printStackTrace();
            //TODO
            throw new NQuadsReaderError();
        }
    }

    private void readEOL() throws NQuadsReaderError {

        lastType = NQuadsTokenType.END_OF_INPUT;
        lastValue = null;

        try {
            reader.mark(1);
            
            int ch = reader.read();
            
            if (ch == '\n' || ch == '\r') {
                return;
            }
            
            reader.reset();
            
        } catch (IOException e) {
            e.printStackTrace();
            //TODO
            throw new NQuadsReaderError();
        }
    }

    private void readWhitespace() throws NQuadsReaderError {

        lastType = NQuadsTokenType.WHITE_SPACE;
        lastValue = null;

        try {
            
            int ch = -1;
            
            do {
                reader.mark(1);
                
                ch = reader.read();
            
            } while (ch == ' ' || ch == '\t');
                
            reader.reset();
                            
        } catch (IOException e) {
            e.printStackTrace();
            //TODO
            throw new NQuadsReaderError();
        }
    }
    
    private void readIriRef() throws NQuadsReaderError {
        
        lastType = NQuadsTokenType.IRI_REF;
        lastValue = null;

        try {
         
            StringBuilder builder = new StringBuilder(256);
            
            int ch = reader.read();

            while (ch != -1) {
                
                if (ch == '>') {
                    lastValue = builder.toString();
                    return;
                }
                
                //TODO validate chars
                
                builder.append((char)ch);
                ch = reader.read();
            }
            throw new NQuadsReaderError();
                            
        } catch (IOException e) {
            e.printStackTrace();
            //TODO
            throw new NQuadsReaderError();
        }
    }

    private void readString() throws NQuadsReaderError {
        
        lastType = NQuadsTokenType.STRING_LITERAL_QUOTE;
        lastValue = null;

        boolean escape = false;
        
        try {

            StringBuilder builder = new StringBuilder(256);
            
            int ch = reader.read();

            while (ch != -1) {

                if (ch == '"' && !escape) {
                    lastValue = builder.toString();
                    return;                    
                }
                
                escape = ch ==  '\\' && !escape;
                
                if (!escape) {
                    builder.append((char)ch);
                }
                ch = reader.read();
            }
            throw new NQuadsReaderError();
                            
        } catch (IOException e) {
            e.printStackTrace();
            //TODO
            throw new NQuadsReaderError();
        }
    }

    protected NQuadsTokenType  getTokenType() {
        return lastType;
    }
    
    
}
