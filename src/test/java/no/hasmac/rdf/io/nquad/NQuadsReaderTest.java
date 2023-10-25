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
package no.hasmac.rdf.io.nquad;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.stream.Stream;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import no.hasmac.rdf.RdfDataset;
import no.hasmac.rdf.io.error.RdfReaderException;
import no.hasmac.rdf.io.nquad.reader.NQuadsReaderTestCase;
import no.hasmac.rdf.io.nquad.reader.NQuadsReaderTestCase.Type;
import no.hasmac.rdf.io.nquad.reader.NQuadsReaderTestSuite;

class NQuadsReaderTest {

    private final static String TEST_SUITE_NAME = "/n-quads-test-suite-20200629.zip";
    private final static String TEST_CASE_BASE_PATH = "nquads-test-suite/";

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testRead(NQuadsReaderTestCase testCase) throws IOException, URISyntaxException {

        assertNotNull(testCase);
        assertNotNull(testCase.getName());
        assertNotNull(testCase.getType());

        final URL zipFileUrl =  (new NQuadsReaderTest()).getClass().getResource(TEST_SUITE_NAME);

        assertNotNull(zipFileUrl);

        try (final ZipFile zip = new ZipFile(new File(zipFileUrl.toURI()))) {

            assertNotNull(zip);

            try (final Reader reader = new InputStreamReader(zip.getInputStream(zip.getEntry(TEST_CASE_BASE_PATH + testCase.getName() + ".nq")))) {

                RdfDataset dataset = (new NQuadsReader(reader)).readDataset();

                assertNotNull(dataset);

                assertEquals(Type.POSITIVE, testCase.getType());

            } catch (RdfReaderException | IllegalArgumentException e) {
                assertEquals(Type.NEGATIVE, testCase.getType());
            }
        }
    }

    static Stream<NQuadsReaderTestCase> data() throws ZipException, IOException, URISyntaxException {
        return (new NQuadsReaderTestSuite(TEST_SUITE_NAME, TEST_CASE_BASE_PATH + "manifest.json")).load();
    }
}
