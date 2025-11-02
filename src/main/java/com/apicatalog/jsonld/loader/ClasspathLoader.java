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

import java.io.IOException;
import java.net.URI;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.tree.io.NodeParser;

public class ClasspathLoader implements DocumentLoader {

    private final NodeParser parser;

    public ClasspathLoader(final NodeParser parser) {
        this.parser = parser;
    }
    
    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        try (final var is = getClass().getResourceAsStream(url.getPath())) {

            var node = parser.read(is);

            return RemoteDocument.of(node, url);                


        } catch (IOException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
    }
}
