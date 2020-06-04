package com.apicatalog.rdf.lang;

import java.util.function.IntPredicate;

public final class RdfGrammar {

    private RdfGrammar() {
    }

    public static final IntPredicate IS_ASCII_ALPHA =
                    ch -> 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'; 

    public static final IntPredicate IS_DIGIT = ch -> '0' <= ch && ch <= '9';

    public static final IntPredicate IS_ASCII_ALPHA_NUM = IS_DIGIT.or(IS_ASCII_ALPHA);
    
    public static final IntPredicate IS_WHITESPACE = ch -> ch == 0x0009 || ch == 0x0020;

    public static final IntPredicate IS_EOL = ch -> ch == 0x0A || ch == 0x0D;
        
    public static final IntPredicate IS_PN_CHARS_BASE = 
                    IS_ASCII_ALPHA.or(ch -> 
                        (0x00C0 <= ch && ch <= 0x00D6)
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
                        );
                    
    public static final IntPredicate IS_PN_CHARS_U = IS_PN_CHARS_BASE.or(ch -> '_' == ch|| ':' == ch);
    
    public static final IntPredicate IS_PN_CHARS = 
                    IS_PN_CHARS_U.or(IS_DIGIT).or(ch ->
                        '-' == ch
                        || 0x00B7 == ch
                        || (0x0300 <= ch && ch <= 0x036F)
                        || (0x203F <= ch && ch <= 0x2040)
                        );  
}
