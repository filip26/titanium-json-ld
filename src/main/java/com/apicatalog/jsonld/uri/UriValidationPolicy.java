package com.apicatalog.jsonld.uri;

public enum UriValidationPolicy {

    /**
     * No validation is performed
     */
    None,

    /**
     * The validation only targets the scheme
     */
    SchemeOnly,

    /**
     * The validation is be fully performed
     */
    Full ;

    /**
     * Method allowing to convert the legacy boolean to the matching policy
     */
    public static UriValidationPolicy of(boolean value) {
        return value ? Full : SchemeOnly ;
    }
}
