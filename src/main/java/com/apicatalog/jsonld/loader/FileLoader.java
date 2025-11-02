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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.tree.io.NodeParser;

public final class FileLoader implements DocumentLoader {

    private final NodeParser reader;

    public FileLoader(NodeParser reader) {
        this.reader = reader;
    }

    @Override
    public Document loadDocument(final URI url, final Options options) throws JsonLdException {

        if (!"file".equalsIgnoreCase(url.getScheme())) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unsupported URL scheme [" + url.getScheme() + "]. FileLoader accepts only file scheme.");
        }

        final File file = new File(url);

        if (!file.canRead()) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "File [" + url + "] is not accessible to read.");
        }


        try (final InputStream is = new FileInputStream(file)) {
            var node = reader.parse(is);
            
            return RemoteDocument.of(node, url);                

        } catch (FileNotFoundException e) {

            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "File not found [" + url + "].");

        } catch (IOException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
}
