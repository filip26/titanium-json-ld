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
 *
 * @see <a href="https://www.w3.org/TR/n-quads/">RDF 1.1. N-Quads</a>
 *
 */
@Deprecated
public class NQuadsWriter implements RdfWriter {

    private final com.apicatalog.rdf.nquads.NQuadsWriter writer;

    public NQuadsWriter(Writer writer) {
        this.writer = new com.apicatalog.rdf.nquads.NQuadsWriter(writer);
    }

    @Override
    public void write(final RdfDataset dataset) throws IOException {
        for (RdfNQuad nquad : dataset.toList()) {
            write(nquad);
        }
    }

    public void write(final RdfNQuad nquad) throws IOException {
        try {
            if (nquad.getObject().isLiteral()) {
                if (nquad.getObject().asLiteral().getLanguage().isPresent()) {
                    writer.quad(nquad.getSubject().getValue(),
                            nquad.getPredicate().getValue(),
                            nquad.getObject().getValue(),
                            nquad.getObject().asLiteral().getLanguage().orElseThrow(),
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
                            langDir[0],
                            langDir.length > 0 ? langDir[1] : null,
                            nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                    return;
                }

                writer.quad(nquad.getSubject().getValue(),
                        nquad.getPredicate().getValue(),
                        nquad.getObject().getValue(),
                        datatype,
                        nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                return;
            }

            writer.quad(nquad.getSubject().getValue(),
                    nquad.getPredicate().getValue(),
                    nquad.getObject().getValue(),
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
