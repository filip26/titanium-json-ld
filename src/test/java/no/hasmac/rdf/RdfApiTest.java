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
package no.hasmac.rdf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLdError;

class RdfApiTest {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/118">Issue #118</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    void testBlankNodeNotation() throws JsonLdError, IOException {

        final RdfResource node = Rdf.createBlankNode("_:bn1");

        assertNotNull(node);
        assertTrue(node.isBlankNode());
        assertEquals("_:bn1", node.getValue());
    }

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/118">Issue #118</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    void testBlankNodeLabel() throws JsonLdError, IOException {

        final RdfResource node = Rdf.createBlankNode("bn1");

        assertNotNull(node);
        assertTrue(node.isBlankNode());
        assertEquals("_:bn1", node.getValue());
    }
}
