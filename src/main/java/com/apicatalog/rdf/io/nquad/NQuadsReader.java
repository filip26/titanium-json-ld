package com.apicatalog.rdf.io.nquad;

import java.io.Reader;
import java.util.Arrays;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.api.Rdf;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.nquad.Tokenizer.Token;
import com.apicatalog.rdf.io.nquad.Tokenizer.TokenType;

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
        
        RdfGraphName graphName = null;
        
        skipWhitespace(0);
        
        if (TokenType.IRI_REF == tokenizer.token().getType()) {

            graphName = Rdf.createGraphName(IRI.create(tokenizer.token().getValue()));
            tokenizer.next();
            skipWhitespace(0);
        }

        if (TokenType.BLANK_NODE_LABEL == tokenizer.token().getType()) {
            
            graphName = Rdf.createGraphName(BlankNode.create("_:".concat(tokenizer.token().getValue())));
            tokenizer.next();
            skipWhitespace(0);
        }

        if (TokenType.END_OF_STATEMENT != tokenizer.token().getType()) {
            unexpected(tokenizer.token());
        }
        
        tokenizer.next();

        return Rdf.createNQuad(subject, predicate, object, graphName);
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
        
        if (TokenType.LANGTAG == tokenizer.token().getType()) {
            
            String langTag = tokenizer.token().getValue();
            
            tokenizer.next();
            
            return Rdf.createObject(Rdf.createLitteral(value.getValue(), langTag));
            
        } else if (TokenType.LITERAL_DATA_TYPE == tokenizer.token().getType()) {
            
            tokenizer.next();
            skipWhitespace(0);
            
            Token attr = tokenizer.token();
                        
            if (TokenType.IRI_REF == attr.getType()) {
                tokenizer.next();
                return Rdf.createObject(Rdf.createLitteral(value.getValue(), IRI.create(attr.getValue())));
            }
                
            unexpected(attr);
        }
        
        return Rdf.createObject(Rdf.createLitteral(value.getValue()));
    }

    
    private <T> T unexpected(Token token, TokenType ...types) throws NQuadsReaderError {
        throw new NQuadsReaderError(
                    "Unexpected token " + token.getType() + "(" + token.getValue() + "). "
                    + "Expected one of " + Arrays.toString(types) + "."
                    );
    }
    
    private IRI readIri()  throws NQuadsReaderError {
        
        Token token = tokenizer.token();
        
        if (TokenType.IRI_REF != token.getType()) {
            unexpected(token, TokenType.IRI_REF);
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
  
//        if (count < min) {
//            unexpected(token);
//        }        
    }


}
