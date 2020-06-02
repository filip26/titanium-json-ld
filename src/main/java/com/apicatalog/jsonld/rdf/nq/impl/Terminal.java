package com.apicatalog.jsonld.rdf.nq.impl;

final class Terminal {

    private Terminal() {
    }

    static final boolean isAsciiAlpha(int ch) {
        return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z';  
    }

    static final boolean isAsciiAlphaNum(int ch) {
        return isDigit(ch) || isAsciiAlpha(ch);  
    }
    
    static final boolean isWhitespace(int ch) {
        return ch == 0x0009 || ch == 0x0020;
    }
    
    static final boolean isDigit(int ch) {
        return '0' <= ch && ch <= '9';
    }

    static final boolean isEol(int ch) {
        return ch == 0x0A || ch == 0x0D;
    }

    static final boolean isPnCharsU(int ch) {
        return isPnCharsBase(ch)
                    || '_' == ch
                    || ':' == ch
                    ;
    }
    
    static final boolean isPnChars(int ch) {
        return isPnCharsU(ch)
                    || '-' == ch
                    || isDigit(ch)
                    || 0x00B7 == ch
                    || (0x0300 <= ch && ch <= 0x036F)
                    || (0x203F <= ch && ch <= 0x2040)
                    ;
    }    
    
    static final boolean isPnCharsBase(int ch) {
        return isAsciiAlpha(ch)
                    || (0x00C0 <= ch && ch <= 0x00D6)
                    || (0x00D8 <= ch && ch <= 0x00F6)
                    || (0x00F8 <= ch && ch <= 0x02FF)
                    || (0x0370 <= ch && ch <= 0x037D)
                    || (0x037F <= ch && ch <= 0x1FFF)
                    || (0x200C <= ch && ch <= 0x200D)
                    || (0x2070 <= ch && ch <= 0x218F)
                    || (0x2C00 <= ch && ch <= 0x2FEF)
                    || (0x3001 <= ch && ch <= 0xD7FF)
                    || (0xF900 <= ch && ch <= 0xFDCF)
                    || (0xFDF0 <= ch && ch <= 0xFFFD)
                    || (0x10000 <= ch && ch <= 0xEFFFF)
                    ;
    }
}
