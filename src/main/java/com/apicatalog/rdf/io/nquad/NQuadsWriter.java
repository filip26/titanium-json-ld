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
package com.apicatalog.rdf.io.nquad;

import java.io.IOException;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.nquads.NQuadsAlphabet;

/**
 * This class is deprecated as of version 1.7.0.
 * <p>
 * Please use
 * <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium RDF
 * Primitives</a> or any other third-party library to materialize RDF
 * primitives.
 * </p>
 *
 * @see <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium
 *      RDF Primitives</a>
 * @see <a href="https://github.com/filip26/titanium-rdf-n-quads">Titanium RDF
 *      N-QUADS</a>
 * @deprecated since 1.7.0 - use an alternative RDF primitives library.
 */
@Deprecated
public class NQuadsWriter implements RdfWriter {

    private final com.apicatalog.rdf.nquads.NQuadsWriter writer;
    private final Writer output;

    public NQuadsWriter(Writer writer) {
        this.writer = new com.apicatalog.rdf.nquads.NQuadsWriter(writer);
        this.output = writer;
    }

    @Override
    public void write(final RdfDataset dataset) throws IOException {
        for (RdfNQuad nquad : dataset.toList()) {
            write(nquad);
        }
        output.flush();
    }

    public void write(final RdfNQuad nquad) throws IOException {
        try {
            if (nquad.getObject().isLiteral()) {
                if (nquad.getObject().asLiteral().getLanguage().isPresent()) {
                    writer.quad(nquad.getSubject().getValue(),
                            nquad.getPredicate().getValue(),
                            nquad.getObject().getValue(),
                            nquad.getObject().asLiteral().getDatatype(),
                            nquad.getObject().asLiteral().getLanguage().get(),
                            null,
                            nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                    return;
                }
                String datatype = nquad.getObject().asLiteral().getDatatype();
                if (datatype.startsWith(RdfConstants.I18N_BASE)) {
                    String[] langDir = datatype.substring(RdfConstants.I18N_BASE.length()).split("_");

                    writer.quad(nquad.getSubject().getValue(),
                            nquad.getPredicate().getValue(),
                            nquad.getObject().getValue(),
                            RdfConstants.I18N_BASE,
                            langDir.length > 0 && langDir[0] != null && !langDir[0].trim().isEmpty()
                                    ? langDir[0]
                                    : null,
                            langDir.length > 1 && langDir[1] != null && !langDir[1].trim().isEmpty()
                                    ? langDir[1]
                                    : null,
                            nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                    return;
                }

                writer.quad(nquad.getSubject().getValue(),
                        nquad.getPredicate().getValue(),
                        nquad.getObject().getValue(),
                        datatype,
                        null,
                        null,
                        nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                return;
            }

            writer.quad(nquad.getSubject().getValue(),
                    nquad.getPredicate().getValue(),
                    nquad.getObject().getValue(),
                    null, null, null,
                    nquad.getGraphName().map(RdfResource::getValue).orElse(null));
        } catch (RdfConsumerException e) {
            if (e.getCause() instanceof IOException) {
                throw (IOException) e.getCause();
            }
            throw new IllegalStateException(e);
        }
    }

    public static final String escape(String value) {
        return NQuadsAlphabet.escape(value);
    }
}
