package com.apicatalog.web.uri;

/**
 * Represents different URI validation policies. These policies determine the
 * extent to which URIs are validated during processing.
 */
public enum UriValidationPolicy {

    /**
     * Disables URI validation entirely, accepting all input as-is. This option
     * provides maximum flexibility but may lead to non-compliant or invalid URI
     * handling.
     */
    None,

    /**
     * Validates only the scheme component of the URI, allowing for more lenient
     * processing while still ensuring a recognizable scheme (e.g., "http:",
     * "https:").
     */
    SchemeOnly,

    /**
     * Performs full URI validation, ensuring that the URI conforms to all syntax
     * and structural requirements defined by relevant standards.
     */
    Full;
}
