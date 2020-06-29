package com.apicatalog.rdf.io.nquad;

import java.io.Reader;
import java.util.Arrays;

import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.nquad.Tokenizer.Token;
import com.apicatalog.rdf.io.nquad.Tokenizer.TokenType;

/**
 * 
 * @see <a href="https://www.w3.org/TR/n-quads/">RDF 1.1. N-Quads</a>
 *
 */
public final class NQuadsReader implements RdfReader {

    private final Tokenizer tokenizer;
    private RdfDataset dataset;
    
    public NQuadsReader(final Reader reader) {
        this.tokenizer = new Tokenizer(reader);
        this.dataset = null;
    }
    
    @Override
    public RdfDataset readDataset() throws NQuadsReaderException {

        if (dataset != null) {
            return dataset;
        }
        
        dataset = Rdf.createDataset();
        
        while (tokenizer.hasNext()) {

            // skip EOL and whitespace
            if (tokenizer.accept(Tokenizer.TokenType.END_OF_LINE)
                    || tokenizer.accept(Tokenizer.TokenType.WHITE_SPACE)
                    || tokenizer.accept(Tokenizer.TokenType.COMMENT)
                    ) {

                continue;
            }
            
            dataset.add(reaStatement());
        }

        return dataset;
    }
    
    private RdfNQuad reaStatement() throws NQuadsReaderException {

        RdfSubject subject = readSubject();
  
        skipWhitespace(0);
  
        String predicate = readIri();

        skipWhitespace(0);
  
        RdfObject object = readObject();

        RdfGraphName graphName = null;
        
        skipWhitespace(0);
        
        if (TokenType.IRI_REF == tokenizer.token().getType()) {

            graphName = Rdf.createGraphName(RdfGraphName.Type.IRI, tokenizer.token().getValue());
            tokenizer.next();
            skipWhitespace(0);
        }

        if (TokenType.BLANK_NODE_LABEL == tokenizer.token().getType()) {
            
            graphName = Rdf.createGraphName(RdfGraphName.Type.BLANK_NODE, "_:".concat(tokenizer.token().getValue()));
            tokenizer.next();
            skipWhitespace(0);
        }

        if (TokenType.END_OF_STATEMENT != tokenizer.token().getType()) {
            unexpected(tokenizer.token(), TokenType.END_OF_STATEMENT);
        }
        
        tokenizer.next();

        skipWhitespace(0);

        // skip comment
        if (TokenType.COMMENT == tokenizer.token().getType()) {
            tokenizer.next();

        // skip end of line
        } else if (TokenType.END_OF_LINE != tokenizer.token().getType() && TokenType.END_OF_INPUT != tokenizer.token().getType()) {
            unexpected(tokenizer.token(), TokenType.END_OF_LINE, TokenType.END_OF_INPUT);
            tokenizer.next();
        }
        
        return Rdf.createNQuad(subject, Rdf.createPredicate(RdfPredicate.Type.IRI, predicate), object, graphName);
    }
    
    private RdfSubject readSubject()  throws NQuadsReaderException {

        final Token token = tokenizer.token();
        
        if (TokenType.IRI_REF == token.getType()) {
            
            tokenizer.next();
            
            return Rdf.createSubject(RdfSubject.Type.IRI, token.getValue());
        } 

        if (TokenType.BLANK_NODE_LABEL == token.getType()) {
            
            tokenizer.next();
            
            return Rdf.createSubject(RdfSubject.Type.BLANK_NODE, "_:".concat(token.getValue()));            
        }
  
        return unexpected(token);
    }
    
    private RdfObject readObject()  throws NQuadsReaderException {
        
        Token token = tokenizer.token();
        
        if (TokenType.IRI_REF == token.getType()) {
            tokenizer.next();
            
            return Rdf.createObject(RdfObject.Type.IRI, token.getValue());
        } 

        if (TokenType.BLANK_NODE_LABEL == token.getType()) {
            
            tokenizer.next();
            
            return Rdf.createObject(RdfObject.Type.BLANK_NODE, "_:".concat(token.getValue()));            
        }
  
        return readLiteral();
    }
    
    private RdfObject readLiteral()  throws NQuadsReaderException {
  
        Token value = tokenizer.token();
        
        if (TokenType.STRING_LITERAL_QUOTE != value.getType()) {
            unexpected(value);
        }

        tokenizer.next();
        
        skipWhitespace(0);
        
        if (TokenType.LANGTAG == tokenizer.token().getType()) {
            
            String langTag = tokenizer.token().getValue();
            
            tokenizer.next();
            
            return Rdf.createObject(Rdf.createLangString(value.getValue(), langTag));
            
        } else if (TokenType.LITERAL_DATA_TYPE == tokenizer.token().getType()) {
            
            tokenizer.next();
            skipWhitespace(0);
            
            Token attr = tokenizer.token();
                        
            if (TokenType.IRI_REF == attr.getType()) {
                tokenizer.next();
                return Rdf.createObject(Rdf.createTypedString(value.getValue(), attr.getValue()));
            }
                
            unexpected(attr);
        }
        
        return Rdf.createObject(RdfObject.Type.LITERAL, value.getValue());
    }

    
    private static final <T> T unexpected(Token token, TokenType ...types) throws NQuadsReaderException {
        throw new NQuadsReaderException(
                    "Unexpected token " + token.getType() + (token.getValue() != null ? "[" + token.getValue() + "]" : "" ) +  ". "
                    + "Expected one of " + Arrays.toString(types) + "."
                    );
    }
    
    private String readIri()  throws NQuadsReaderException {
        
        Token token = tokenizer.token();
        
        if (TokenType.IRI_REF != token.getType()) {
            unexpected(token, TokenType.IRI_REF);
        }
        
        tokenizer.next();
        
        return token.getValue();
    }


    private void skipWhitespace(int min) throws NQuadsReaderException {
  
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
