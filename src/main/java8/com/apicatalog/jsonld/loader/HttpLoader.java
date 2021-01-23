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
package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.Collection;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

public class HttpLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(HttpLoader.class.getName());

    public static final int MAX_REDIRECTIONS = 10;
    
    private static final HttpLoader INSTANCE = new HttpLoader();


    private static final String PLUS_JSON = "+json";
    
    private final int maxRedirections;

    
    public HttpLoader() {
        this(MAX_REDIRECTIONS);
    }
    
    public HttpLoader(int maxRedirections) {
        this.maxRedirections = maxRedirections;
    }

    public static final DocumentLoader defaultInstance() {
        return INSTANCE;
    }

    @Override
    public Document loadDocument(final URI uri, final DocumentLoaderOptions options) throws JsonLdError {
        throw new UnsupportedOperationException();  //TODO use OkHtttp
    }

    public static final String getAcceptHeader() {
        return getAcceptHeader(null);
    }
    
    public static final String getAcceptHeader(final Collection<String> profiles) {
        final StringBuilder builder = new StringBuilder();
        
        builder.append(MediaType.JSON_LD.toString());

        if (profiles != null && !profiles.isEmpty()) {
            builder.append(";profile=\"");
            builder.append(String.join(" ", profiles));
            builder.append("\"");
        }
        
        builder.append(',');
        builder.append(MediaType.JSON.toString());
        builder.append(";q=0.9,*/*;q=0.8"); 
        return builder.toString();        
    }    
}
