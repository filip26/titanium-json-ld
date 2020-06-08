package com.apicatalog.rdf.io.nquad;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;

import com.apicatalog.rdf.lang.RdfGrammar;

/**
 * 
 * @see <a href="https://www.w3.org/TR/n-quads/#sec-grammar">N-Quads Grammar</a>
 *
 */
final class Tokenizer {

    private static final int BUFFER_SIZE = 8192;
    
    private final Reader reader;
    
    private Token next;
    
    protected Tokenizer(Reader reader) {
        this.reader = new BufferedReader(reader, BUFFER_SIZE);
        this.next = null;
    }
    
    public Token next() throws NQuadsReaderException {
        
        if (!hasNext()) {
            return next;
        }
        
        next = doRead();
        return next;
    }
    
    public Token token() throws NQuadsReaderException {        
        hasNext();
        return next;
    }
    
    public boolean accept(TokenType type) throws NQuadsReaderException {
        if (type == token().getType()) {
            next();
            return true;
        }
        return false;
    }     
    
    private Token doRead() throws NQuadsReaderException {
        
        try {
            int ch = reader.read();
            
            if (ch == -1) {
                return Token.EOI;
            }
            
            // WS
            if (RdfGrammar.IS_WHITESPACE.test(ch)) {                
                return skipWhitespaces();  
            }
            
            if (ch == '<') {
                return readIriRef();
            }

            if (ch == '"') {
                return readString();
            }
            
            if (ch == '.') {
                return Token.EOS;
            }
            
            if (RdfGrammar.IS_EOL.test(ch)) {
                return skipEol();
            }
            
            if (ch == '@') {
                return readLangTag();
            }
            
            if (ch == '_') {
                return readBlankNode();
            }
            
            if (ch == '^') {
                
                ch = reader.read();
                
                if ('^' != ch) {
                    unexpected(ch, "^");
                }
                return Token.LITERAL_DATA_TYPE;
            }

            unexpected(ch, "\\\t", "\\\n", "\\\r", "^", "@", "SPACE", ".", "<", "_", "\"");
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }
        
        throw new IllegalStateException();
    }
    
    private void unexpected(int actual, String ...expected) throws NQuadsReaderException {
        throw new NQuadsReaderException(
                        actual != -1 
                            ? "Unexpected character [" + (char)actual  + "] expected " +  Arrays.toString(expected) + "." 
                            : "Unexpected end of input, expected " + Arrays.toString(expected) + "."
                            );
    }
    
    private Token skipWhitespaces() throws NQuadsReaderException {

        try {
            reader.mark(1);
            int ch = reader.read();
            
            while (RdfGrammar.IS_WHITESPACE.test(ch)) {
                reader.mark(1);
                ch = reader.read();
            }
            
            reader.reset();

            return Token.WS;
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }
    }

    private Token skipEol() throws NQuadsReaderException {

        try {
            reader.mark(1);
            int ch = reader.read();
            
            while (RdfGrammar.IS_EOL.test(ch)) {
                reader.mark(1);
                ch = reader.read();
            }
            
            reader.reset();

            return Token.EOL;
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }
    }

    private Token readIriRef() throws NQuadsReaderException {

        try {

            StringBuilder value = new StringBuilder();
            
            int ch = reader.read();
            
            while (ch != '>'  && ch != -1) {
                
                if ((0x00 <= ch && ch <= 0x20)
                     || ch == '<'
                     || ch == '"'
                     || ch == '{'
                     || ch == '}'
                     || ch == '|'
                     || ch == '^'
                     || ch == '`'
                        ) {
                    unexpected(ch, ">");
                }
                
                if (ch == '\\') {
                                        
                    ch = reader.read();

                    if (ch == 'u') {
                     
                        value.append(readUnicode());

                    } else if (ch == 'U') {
                        
                        //TODO unicode
                        unexpected(ch);
                        
                    } else {
                        unexpected(ch);
                    }
                    
                } else {
                    value.append((char)ch);
                }
                ch = reader.read();
            }
            
            if (ch == -1) {
                unexpected(ch);
            }

            return new Token(TokenType.IRI_REF, value.toString());
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }        
    }
    
    private Token readString() throws NQuadsReaderException {
        try {

            StringBuilder value = new StringBuilder();
            
            int ch = reader.read();
            
            while (ch != '"' && ch != -1) {
                
                if (ch == 0x22
                     || ch == 0xa
                     || ch == 0xd
                        ) {
                    unexpected(ch);
                }
                
                if (ch == '\\') {

                    ch = reader.read();
                    
                    if (ch == 't' || ch == 'b' || ch == 'n' || ch == 'r' || ch == 'f' || ch == '\'' || ch == '\\' || ch =='"') {

                        value.appendCodePoint(unescape(ch));

                    } else if (ch == 'u') {

                        value.append(readUnicode());
                        
                    } else if (ch == 'U') {
                        
                        //TODO unescape                        
                        unexpected(ch);
                        
                    } else {
                        unexpected(ch);
                    }
                    
                } else {
                    value.appendCodePoint(ch);
                }
                ch = reader.read();
            }
            
            if (ch == -1) {
                unexpected(ch);
            }

            return new Token(TokenType.STRING_LITERAL_QUOTE, value.toString());
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }    
    }

    private Token readLangTag() throws NQuadsReaderException {
        try {

            StringBuilder value = new StringBuilder();
            
            int ch = reader.read();
            
            if (!RdfGrammar.IS_ASCII_ALPHA.test(ch) || ch == -1) {
                unexpected(ch);
            }
            value.append((char)ch);
            
            reader.mark(1);
            ch = reader.read();
            
            while (RdfGrammar.IS_ASCII_ALPHA.test(ch)) {
                
                value.append((char)ch);
                
                reader.mark(1);
                ch = reader.read();
            }
            
            if (ch == -1) {
                unexpected(ch);
            }
            
            boolean delim = false;
            
            while (RdfGrammar.IS_ASCII_ALPHA_NUM.test(ch) || ch == '-') {

                value.append((char)ch);
                
                reader.mark(1);
                ch = reader.read();

                delim = ch == '-';
            }
            
            if (ch == -1 || delim) {
                unexpected(ch);
            }
            
            reader.reset();

            return new Token(TokenType.LANGTAG, value.toString());
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }    
    }

    private Token readBlankNode() throws NQuadsReaderException {
        try {

            StringBuilder value = new StringBuilder();
            
            int ch = reader.read();
            
            if (ch != ':' || ch == -1) {
                unexpected(ch);
            }
            
            ch = reader.read();
            
            if (RdfGrammar.IS_PN_CHARS_U.negate().and(RdfGrammar.IS_ASCII_DIGIT.negate()).test(ch) || ch == -1) {
                unexpected(ch);
            }
            
            value.append((char)ch);
            
            reader.mark(1);
            ch = reader.read();
            
            boolean delim = false;
            
            while (RdfGrammar.IS_PN_CHARS.test(ch) || ch == '.') {

                value.append((char)ch);
                
                reader.mark(1);
                ch = reader.read();

                delim = ch == '.';
            }
            
            if (ch == -1 || delim) {
                unexpected(ch);
            }
            
            reader.reset();

            return new Token(TokenType.BLANK_NODE_LABEL, value.toString());
            
        } catch (IOException e) {
            throw new NQuadsReaderException(e);
        }    
    }
    
    private char[] readUnicode() throws NQuadsReaderException, IOException {
        
        char[] code = new char[4];
        
        code[0] = readHex8();
        code[1] = readHex8();
        code[2] = readHex8();
        code[3] = readHex8();

        return Character.toChars(Integer.parseInt(String.valueOf(code), 16));
    }

    private char readHex8()  throws IOException, NQuadsReaderException {
        
        int hex = reader.read();
        
        if (RdfGrammar.IS_HEX.negate().test(hex)) {
            unexpected(hex, "0-9", "a-f", "A-F");
        }
        return (char)hex;
    }
    
    private static final int unescape(int symbol) {
        if (symbol == 't') {
            return 0x9;
            
        } else if (symbol == 'b') {
            return 0x8;
            
        } else if (symbol == 'n') {
            return 0xa;
            
        } else if (symbol == 'r') {
            return 0xd;
            
        } else if (symbol == 'f') {
            return 0xc;
        }
        return symbol;        
    }

    public boolean hasNext() throws NQuadsReaderException {
        if (next == null) {
            next = doRead();
        }
        return TokenType.END_OF_INPUT != next.getType();
    }
    
    protected static class Token {
        
        static final Token EOI = new Token(TokenType.END_OF_INPUT, null);
        static final Token EOS = new Token(TokenType.END_OF_STATEMENT, null);
        static final Token EOL = new Token(TokenType.END_OF_LINE, null);
        static final Token WS = new Token(TokenType.WHITE_SPACE, null);
        
        static final Token LITERAL_DATA_TYPE = new Token(TokenType.LITERAL_DATA_TYPE, null);
        
        final TokenType type;
        final String value;
        
        Token(TokenType type, String value) {
            this.type = type;
            this.value = value;
        }
        
        TokenType getType() {
            return type;
        }

        String getValue() {
            return value;
        }
    }
    
    protected enum TokenType {
        LANGTAG,
        IRI_REF,
        STRING_LITERAL_QUOTE,
        BLANK_NODE_LABEL,
        WHITE_SPACE,
        LITERAL_DATA_TYPE,
        END_OF_STATEMENT,
        END_OF_LINE,
        END_OF_INPUT,
    }
}

