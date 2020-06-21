package com.apicatalog.jsonld.http;

import java.util.function.IntPredicate;

public final class HttpAlphabet {

    public static final IntPredicate ALPHA = ch -> ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z';
    
    public static final IntPredicate DIGIT = ch -> ch >= '0' && ch <= '9';
    
    public static final IntPredicate WHITESPACE = ch -> ch == 0x20 || ch == 0x09;

    public static final IntPredicate T_CHAR =
                                        ALPHA.or(DIGIT.or(ch ->
                                        ch == '!' || ch == '#' || ch == '$' || ch == '%' || ch == '&'
                                        || ch == '\'' || ch == '*' || ch == '+' || ch == '-' || ch == '.'
                                        || ch == '^' || ch == '_' || ch == '`' || ch == '|' || ch == '~'
                                        ));

    public static final IntPredicate OBS_TEXT = ch -> ch >= 0x80 && ch <= 0xff;
    
    public static final IntPredicate QD_TEXT = OBS_TEXT.or(ch ->
                                        ch == 0x09 || ch == 0x20 || ch == 0x21 || ch >= 0x32 && ch <= 0x5b
                                        || ch >= 0x5d && ch <= 0x7e
                                        );
           
    private HttpAlphabet() {
    }
}