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
package no.hasmac.rdf.io.nquad.reader;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import no.hasmac.jsonld.json.JsonUtils;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.stream.JsonParser;

public final class NQuadsReaderTestSuite {

    private final String filePath;
    private final String manifestName;

    public NQuadsReaderTestSuite(final String filePath, final String manifestName) {
        this.filePath = filePath;
        this.manifestName = manifestName;
    }

    public final Stream<NQuadsReaderTestCase> load() throws ZipException, IOException, URISyntaxException {

        final URL zipFileUrl =  NQuadsReaderTestSuite.class.getResource(filePath);

        assertNotNull(zipFileUrl);

        try (final ZipFile zip = new ZipFile(new File(zipFileUrl.toURI()))) {

            final ZipEntry manifestEntry = zip.getEntry(manifestName);

            try (final InputStream is = zip.getInputStream(manifestEntry)) {

                final JsonParser parser = Json.createParser(is);

                parser.next();

                return parser
                            .getArray()
                            .stream()
                            .filter(JsonUtils::isObject)
                            .map(JsonObject.class::cast)
                            .map(NQuadsReaderTestCase::of);
            }
        }
    }
}
