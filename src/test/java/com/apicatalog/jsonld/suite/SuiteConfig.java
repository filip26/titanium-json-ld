/*
 * Copyright 2020-2026 the original author or authors.
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
package com.apicatalog.jsonld.suite;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.time.Duration;

import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.UriFilter;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.jakcson.Jackson2Parser;
import com.apicatalog.web.media.MediaType;
import com.fasterxml.jackson.core.JsonFactory;

public class SuiteConfig {

    public static DocumentLoader LOADER = UriFilter.newBlacklist(new ZipResourceLoader(SuiteConfig::read))
            .base("http:")
            .base("https:")
            .build();
    
    public static final JsonFactory JSON_FACTORY = JsonFactory.builder()
            .build();

    public static final HttpClient HTTP_CLIENT = HttpClient
            .newBuilder()
            .followRedirects(Redirect.NEVER)
            .connectTimeout(Duration.ofSeconds(5))
            .build();

    public static final HttpLoader HTTP_LOADER = HttpLoader
            .newInstance(HTTP_CLIENT, SuiteConfig::read)
            .timeout(Duration.ofSeconds(5));

    public static final Object read(MediaType contentType, Reader reader) {
        try (var parser = new Jackson2Parser(JSON_FACTORY.createParser(reader))) {
            return Tree.read(parser);

        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
