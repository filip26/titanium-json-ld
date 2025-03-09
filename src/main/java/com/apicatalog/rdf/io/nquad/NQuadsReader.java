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

import java.io.Reader;

import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfDatasetSupplier;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.error.RdfReaderException;
import com.apicatalog.rdf.nquads.NQuadsReaderException;

/**
 *
 * @see <a href="https://www.w3.org/TR/n-quads/">RDF 1.1. N-Quads</a>
 *
 */
@Deprecated
public class NQuadsReader implements RdfReader {

    protected final com.apicatalog.rdf.nquads.NQuadsReader reader;

    public NQuadsReader(final Reader reader) {
        this.reader = new com.apicatalog.rdf.nquads.NQuadsReader(reader);
    }

    @Override
    public RdfDataset readDataset() throws RdfReaderException {

        RdfDataset dataset = Rdf.createDataset();

        RdfQuadConsumer consumer = new RdfDatasetSupplier(dataset);

        try {
            reader.provide(consumer);
            return dataset;

        } catch (NQuadsReaderException | RdfConsumerException e) {
            throw new RdfReaderException(e);
        }
    }
}
