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
import com.apicatalog.jsonld.rdf.nq.impl.Tokenizer.Token;
import com.apicatalog.jsonld.rdf.nq.impl.Tokenizer.TokenType;

public final class NQuadsReader implements RdfReader {

    private final Tokenizer tokenizer;
    
    public NQuadsReader(final Reader reader) {
        this.tokenizer = new Tokenizer(reader);
    }
    
    @Override
    public RdfDataset readDataset() throws NQuadsReaderError {

        //TODO cache
        
        RdfDataset dataset = Rdf.createDataset();
        
        while (tokenizer.hasNext()) {

            // skip EOL and whitespace
            if (tokenizer.accept(Tokenizer.TokenType.END_OF_LINE)
                    || tokenizer.accept(Tokenizer.TokenType.WHITE_SPACE)
                    ) {

                continue;
            }
            
            dataset.add(reaStatement());
        }

        return dataset;
    }
    
    public RdfNQuad reaStatement() throws NQuadsReaderError {
  
        RdfSubject subject = readSubject();
  
        skipWhitespace(1);
  
        IRI predicate = readIri();

        skipWhitespace(1);
  
        RdfObject object = readObject();
        
        skipWhitespace(0);
  
//        if (!scanner.accept(NQuadsTokenType.END_OF_STATEMENT)) {
//            //TODO read graph name
//        }

        if (TokenType.END_OF_STATEMENT != tokenizer.token().getType()) {
            unexpected(tokenizer.token());
        }
        
        tokenizer.next();

        return Rdf.createNQuad(subject, predicate, object, null);
    }
    
    private RdfSubject readSubject()  throws NQuadsReaderError {

        final Token token = tokenizer.token();
        
        if (TokenType.IRI_REF == token.getType()) {
            
            tokenizer.next();
            
            return Rdf.createSubject(IRI.create(token.getValue()));
        } 

        if (TokenType.BLANK_NODE_LABEL == token.getType()) {
            
            tokenizer.next();
            
            return Rdf.createSubject(BlankNode.create("_:".concat(token.getValue())));            
        }
  
        return unexpected(token);
    }
    
    private RdfObject readObject()  throws NQuadsReaderError {
        
        Token token = tokenizer.token();
        
        if (TokenType.IRI_REF == token.getType()) {
            tokenizer.next();
            
            return Rdf.createObject(IRI.create(token.getValue()));
        } 

        if (TokenType.BLANK_NODE_LABEL == token.getType()) {
            
            tokenizer.next();
            
            return Rdf.createObject(BlankNode.create("_:".concat(token.getValue())));            
        }
  
        return readLiteral();
    }
    
    public RdfObject readLiteral()  throws NQuadsReaderError {
  
        Token value = tokenizer.token();
        
        if (TokenType.STRING_LITERAL_QUOTE != value.getType()) {
            unexpected(value);
        }

        tokenizer.next();
        
        skipWhitespace(0);
        
        if (TokenType.LITERAL_ATTR == tokenizer.token().getType()) {
            
        }
        
        return Rdf.createObject(Rdf.createLitteral(value.getValue()));
    }

    
    private <T> T unexpected(Token token) throws NQuadsReaderError {
        throw new NQuadsReaderError("Unexpected token '" + token.getValue() + "'.");
    }
    
    private IRI readIri()  throws NQuadsReaderError {
        
        Token token = tokenizer.token();
        
        if (TokenType.IRI_REF != token.getType()) {
            unexpected(token);
        }
        
        tokenizer.next();
        
        return IRI.create(token.getValue());
    }


    private void skipWhitespace(int min) throws NQuadsReaderError {
  
        Token token = tokenizer.token();
        
        int count = 0;
        
        while (TokenType.WHITE_SPACE ==  token.getType()) {
            token = tokenizer.next();
            count++;
        }
  
        if (count < min) {
            unexpected(token);
        }        
    }


}
