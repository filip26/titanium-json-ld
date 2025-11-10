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

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;

public class ZipResourceLoader implements DocumentLoader {

    private final TreeParser reader;

    public ZipResourceLoader(TreeParser reader) {
        this.reader = reader;
    }

    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        if (!"zip".equals(url.getScheme())) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED);
        }

        URL zipFileUrl = getClass().getResource("/" + url.getAuthority());

        if (zipFileUrl == null) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED);
        }

        File zipFile = null;

        try {
            zipFile = new File(zipFileUrl.toURI());

        } catch (URISyntaxException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }

        try (ZipFile zip = new ZipFile(zipFile)) {

            ZipEntry zipEntry = zip.getEntry(url.getPath().substring(1));

            if (zipEntry == null) {
                return null;
            }

            try (final InputStream is = zip.getInputStream(zipEntry)) {

                var node = reader.parse(is);

                return Document.of(node, url);
            }

        } catch (IOException | TreeIOException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    public static byte[] fetchBytes(URI url) throws JsonLdException {

        if (!"zip".equals(url.getScheme())) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "url = %s".formatted(url));
        }

        URL zipFileUrl = ZipResourceLoader.class.getResource("/" + url.getAuthority());

        if (zipFileUrl == null) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED);
        }

        File zipFile = null;

        try {
            zipFile = new File(zipFileUrl.toURI());

        } catch (URISyntaxException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }

        try (ZipFile zip = new ZipFile(zipFile)) {

            ZipEntry zipEntry = zip.getEntry(url.getPath().substring(1));

            if (zipEntry == null) {
                return null;
            }

            try (InputStream is = zip.getInputStream(zipEntry)) {

                return is.readAllBytes();
            }

        } catch (IOException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
}
