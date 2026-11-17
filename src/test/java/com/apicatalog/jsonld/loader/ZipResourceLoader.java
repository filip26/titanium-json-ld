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
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.LoaderException.ErrorCode;

public class ZipResourceLoader implements DocumentLoader {

    private final Document.ContentReader parser;

    public ZipResourceLoader(Document.ContentReader parser) {
        this.parser = parser;
    }

    @Override
    public Document loadDocument(URI uri, Options options) throws LoaderException {

        if (!"zip".equals(uri.getScheme())) {
            throw new LoaderException(ErrorCode.UNSUPPORTED_SCHEME, uri);
        }

        URL zipFileUrl = getClass().getResource("/" + uri.getAuthority());

        if (zipFileUrl == null) {
            return null;
        }

        File zipFile = null;

        try {
            zipFile = new File(zipFileUrl.toURI());

        } catch (URISyntaxException e) {
            throw new LoaderException(ErrorCode.INVALID_URI, uri, e);
        }

        try (ZipFile zip = new ZipFile(zipFile)) {

            ZipEntry zipEntry = zip.getEntry(uri.getPath().substring(1));

            if (zipEntry == null) {
                return null;
            }

            try (final var is = new InputStreamReader(zip.getInputStream(zipEntry), StandardCharsets.UTF_8)) {

                var node = parser.read(null, is);

                return new Document(uri, node, null, null, null);
            }

        } catch (IOException | UncheckedIOException e) {
            throw new LoaderException(uri, e);
        }
    }

    public static byte[] fetchBytes(URI uri) throws LoaderException {

        if (!"zip".equals(uri.getScheme())) {
            throw new LoaderException(ErrorCode.UNSUPPORTED_SCHEME, uri, "url = %s".formatted(uri));
        }

        URL zipFileUrl = ZipResourceLoader.class.getResource("/" + uri.getAuthority());

        if (zipFileUrl == null) {
            return null;
        }

        File zipFile = null;

        try {
            zipFile = new File(zipFileUrl.toURI());

        } catch (URISyntaxException e) {
            throw new LoaderException(uri, e);
        }

        try (final var zip = new ZipFile(zipFile)) {

            final var zipEntry = zip.getEntry(uri.getPath().substring(1));

            if (zipEntry == null) {
                return null;
            }

            try (final var is = zip.getInputStream(zipEntry)) {

                return is.readAllBytes();
            }

        } catch (IOException | IllegalStateException e) {
            throw new LoaderException(uri, e);
        }
    }
}
