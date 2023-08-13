package com.apicatalog.jsonld.uri;

class PartiallyImplementedUriValidator {

    private static final String ALPHA_STRING = "abcdefghijklmnopqrstuvwxyz";
    private static final String ALPHA_STRING_UPPER_CASE = ALPHA_STRING.toUpperCase();

    private static final String DIGIT_STRING = "0123456789";
    private static final String PCHAR_EXCEPT_PCT_ENDODED_STRING = ALPHA_STRING_UPPER_CASE + ALPHA_STRING + DIGIT_STRING + "-._~" + "!$&'()*+,;=" + ":@";


    private static final boolean[] ALPHA = toBooleanArray(ALPHA_STRING);
    private static final boolean[] COLON = toBooleanArray(":");
    private static final boolean[] SLASH = toBooleanArray("/");
    private static final boolean[] QUERY_START = toBooleanArray("?");
    private static final boolean[] FRAGMENT_START = toBooleanArray("#");
    private static final boolean[] PCT_ENCODED_START = toBooleanArray("%");
    private static final boolean[] HEXDIG = toBooleanArray(DIGIT_STRING + "abcdefABCDEF");
    private static final boolean[] PCHAR_EXCEPT_PCT_ENCODED = toBooleanArray(PCHAR_EXCEPT_PCT_ENDODED_STRING);

    private static final boolean[] FRAGMENT = toBooleanArray(PCHAR_EXCEPT_PCT_ENDODED_STRING + "/?");
    private static final boolean[] QUERY = toBooleanArray(PCHAR_EXCEPT_PCT_ENDODED_STRING + "/?");


    private static final boolean[] SCHEME = toBooleanArray(ALPHA_STRING + DIGIT_STRING + "+-.");
    private static final boolean[] UNRESERVED = toBooleanArray(ALPHA_STRING + DIGIT_STRING + "-._~");

    private static boolean[] toBooleanArray(String s) {

        boolean[] result = new boolean[256];

        s.codePoints().forEach(c -> {
            result[c] = true;
        });

        return result;

    }


    /**
     * @param uri
     * @return may return false even though the uri is valid and absolute, but will only return true if it is valid and absolute
     */
    public static boolean isDefinitivelyValidAbsoluteUri(String uri) {
        if (uri == null || uri.length() == 0) {
            return false;
        }


        int index = 0;
        boolean valid;

        // scheme
        valid = matches(uri, index++, ALPHA);
        if (!valid) {
            return false;
        }

        while (matches(uri, index, SCHEME)) {
            index++;
        }

        valid = matches(uri, index++, COLON);
        if (!valid) {
            return false;
        }

        if (matches(uri, index, SLASH) && matches(uri, index + 1, SLASH)) {
            index += 2;

            // host
            valid = matches(uri, index++, UNRESERVED);
            if (!valid) {
                return false;
            }

            while (matches(uri, index, UNRESERVED)) {
                index++;
            }

        }

        // simplest to just require at least one character for the path
        if (!(matches(uri, index, PCHAR_EXCEPT_PCT_ENCODED) || matches(uri, index, SLASH))) {
            return false;
        } else {
            // handle path
            index++;
            if (matches(uri, index, SLASH)) {
                return false;
            }

            while (true) {
                if (matches(uri, index, PCHAR_EXCEPT_PCT_ENCODED)) {
                    index++;
                } else if (matches(uri, index, SLASH)) {
                    index++;
                } else if (matches(uri, index, PCT_ENCODED_START)) {
                    // handle percent encoded
                    index++;
                    boolean doubleHex = matches(uri, index++, HEXDIG) && matches(uri, index++, HEXDIG);
                    if (!doubleHex) {
                        return false;
                    }
                } else {
                    break;
                }
            }

        }

        if (matches(uri, index, QUERY_START)) {
            index++;
            while (true) {
                if (matches(uri, index, QUERY)) {
                    index++;
                } else if (matches(uri, index, PCT_ENCODED_START)) {
                    // handle percent encoded
                    index++;
                    boolean doubleHex = matches(uri, index++, HEXDIG) && matches(uri, index++, HEXDIG);
                    if (!doubleHex) {
                        return false;
                    }
                } else {
                    break;
                }
            }
        }


        if (matches(uri, index, FRAGMENT_START)) {
            index++;
            while (true) {
                if (matches(uri, index, FRAGMENT)) {
                    index++;
                } else if (matches(uri, index, PCT_ENCODED_START)) {
                    // handle percent encoded
                    index++;
                    boolean doubleHex = matches(uri, index++, HEXDIG) && matches(uri, index++, HEXDIG);
                    if (!doubleHex) {
                        return false;
                    }
                } else {
                    break;
                }
            }
        }


        return noMoreCodepoints(uri, index);
    }

    private static boolean noMoreCodepoints(String s, int index) {
        return index == s.length();
    }

    private static boolean matches(String s, int i, boolean[] lookupArray) {
        if (i >= s.length()) {
            return false;
        }


        if (s.codePointAt(i) >= lookupArray.length) {
            return false;
        }

        boolean valid = lookupArray[s.codePointAt(i)];
//        if (valid) {
//            String string = Character.toString(codePoints[i]);
//            System.out.println(string);
//        }
        return valid;
    }


}
