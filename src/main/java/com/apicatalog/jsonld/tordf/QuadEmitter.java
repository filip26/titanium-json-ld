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
package com.apicatalog.jsonld.tordf;

import com.apicatalog.jsonld.tordf.JsonLdToQuads.QuadConsumer;

class QuadEmitter implements TripleConsumer {

    private final QuadConsumer consumer;
    private String graph;

    QuadEmitter(QuadConsumer consumer) {
        this.consumer = consumer;
        this.graph = null;
    }

    @Override
    public void defaultGraph() {
        this.graph = null;
    }

    @Override
    public void namedGraph(String graph) {
        this.graph = graph;
    }

    @Override
    public void triple(String subject, String predicate, String object) {
        consumer.quad(subject, predicate, object, null, null, null, graph);
    }

    @Override
    public void triple(String subject, String predicate, String literal, String datatype) {
        consumer.quad(subject, predicate, literal, datatype, null, null, graph);
    }

    @Override
    public void triple(
            String subject,
            String predicate,
            String literal,
            String datatype,
            String language,
            String direction) {
        consumer.quad(subject, predicate, literal, datatype, language, direction, graph);
    }
}
