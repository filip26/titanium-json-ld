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
package no.hasmac.rdf.impl;

import no.hasmac.rdf.RdfResource;
import no.hasmac.rdf.RdfTriple;
import no.hasmac.rdf.RdfValue;

class RdfTripleImpl implements RdfTriple {

    private final RdfResource subject;

    private final RdfResource predicate;

    private final RdfValue object;

    protected RdfTripleImpl(final RdfResource subject, final RdfResource predicate, final RdfValue object) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
    }

    @Override
    public RdfResource getSubject() {
        return subject;
    }

    @Override
    public RdfResource getPredicate() {
        return predicate;
    }

    @Override
    public RdfValue getObject() {
        return object;
    }

    @Override
    public String toString() {
        return  "RdfTripleImpl[subject=" + subject
                    + ", predicate=" + predicate
                    + ", object=" + object
                    + "]";
    }
}
