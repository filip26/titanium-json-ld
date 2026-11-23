/*
 * Copyright 2020-2026 the original author or authors.
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
package com.apicatalog.rdf;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Set;

import com.apicatalog.rdf.canon.RdfCanon;
import com.apicatalog.rdf.canon.RdfCanonTimeTicker;
import com.apicatalog.rdf.model.RdfQuad;
import com.apicatalog.rdf.nquads.NQuadsWriter;
import com.apicatalog.rdf.provider.QuadEmitter;

public final class RdfComparison {

    public static final boolean equals(final Set<RdfQuad> dataset1, final Set<RdfQuad> dataset2) throws IOException {
        return canon(dataset1).equals(canon(dataset2));
    }

    private static final String canon(Set<RdfQuad> set) throws IOException {

        var canon = RdfCanon.create("SHA-256", new RdfCanonTimeTicker(5 * 1000));

        QuadEmitter.create(canon).emit(set);

        var writer = new StringWriter();

        try (var nquads = new NQuadsWriter(writer)) {
            canon.provide(nquads::quad);
        }

        return writer.toString();
    }
}
