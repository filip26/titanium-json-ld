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
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.tree.io.NodeReader;

public class ZipResourceLoader implements DocumentLoader {

    private final NodeReader reader;

    public ZipResourceLoader(NodeReader reader) {
        this.reader = reader;
    }

    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        if (!"zip".equals(url.getScheme())) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        URL zipFileUrl = getClass().getResource("/" + url.getAuthority());

        if (zipFileUrl == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        File zipFile = null;

        try {
            zipFile = new File(zipFileUrl.toURI());

        } catch (URISyntaxException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }

        try (ZipFile zip = new ZipFile(zipFile)) {

            ZipEntry zipEntry = zip.getEntry(url.getPath().substring(1));

            if (zipEntry == null) {
                return null;
            }

//            
//            
//            final DocumentReader<InputStream> reader;
//
//            if (zipEntry.getName().endsWith(".nq")) {
//                try (final InputStream is = zip.getInputStream(zipEntry)) {
//
//                    final Document document = QuadSetDocument.readNQuads(new InputStreamReader(is));
//                    document.setDocumentUrl(url);
//
//                    return document;
//                }
//
//            } else if (zipEntry.getName().endsWith(".json")) {
//                reader = resolver.getReader(MediaType.JSON);
//
//            } else if (zipEntry.getName().endsWith(".jsonld")) {
//                reader = resolver.getReader(MediaType.JSON_LD);
//
//            } else {
//                return null;
//            }

            try (final InputStream is = zip.getInputStream(zipEntry)) {

                var node = reader.read(is);
                
                return RemoteDocument.of(node, url);                
            }

        } catch (IOException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
}
