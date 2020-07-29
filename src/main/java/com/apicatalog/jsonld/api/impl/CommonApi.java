/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
     * @param processingMode to set
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
