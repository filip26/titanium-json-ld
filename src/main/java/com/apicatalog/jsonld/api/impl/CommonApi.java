package com.apicatalog.jsonld.api.impl;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.lang.Version;

public interface CommonApi<R> {

    /**
     * Override an existing settings with {@link JsonLdOptions}.
     * 
     * @param options {@link JsonLdOptions}
     * @return builder instance 
     */
    R options(JsonLdOptions options);

    /**
     * Set <code>JSON-LD</code> processing mode. JSON-LD 1.1 is set by default.
     *  
     * @param processingMode
     * @return builder instance
     */
    R mode(Version processingMode);

    /**
     * Set the base <code>IRI</code>. If set, this overrides the input document's IRI.
     * 
     * @param baseUri
     * @return builder instance
     */
    R base(URI baseUri);        

    /**
     * Set the base {@link URI}. If set, this overrides the input document's IRI.
     *
     * @param baseLocation
     * @return builder instance
     */
    R base(String baseLocation);

    /**
     * If set to <code>true</code>, certain algorithm processing steps
     * are ordered lexicographically. If <code>false</code>, order is not
     * considered in processing.
     * 
     * @param enable
     * @return builder instance
     */
    R ordered(boolean enable);

    /**
     * Certain algorithm processing steps are ordered lexicographically.
     * 
     * @return builder instance
     */
    R ordered();

}
