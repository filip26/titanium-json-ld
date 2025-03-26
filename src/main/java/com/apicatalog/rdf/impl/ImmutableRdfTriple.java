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
package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;

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
 * @deprecated since 1.7.0 - use an alternative RDF primitives library.
 */
@Deprecated
class ImmutableRdfTriple implements RdfTriple {

    private final RdfResource subject;

    private final RdfResource predicate;

    private final RdfValue object;

    protected ImmutableRdfTriple(final RdfResource subject, final RdfResource predicate, final RdfValue object) {
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
        return subject + " " + predicate + " " + object + " .";
    }

    @Override
    public int hashCode() {
        return Objects.hash(object, predicate, subject);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() == obj.getClass()) {
            ImmutableRdfTriple other = (ImmutableRdfTriple) obj;
            return Objects.equals(object, other.object) && Objects.equals(predicate, other.predicate) && Objects.equals(subject, other.subject);
        }
        if (!(obj instanceof RdfTriple)) {
            return false;
        }
        RdfTriple other = (RdfTriple) obj;
        return Objects.equals(predicate, other.getPredicate())
                && Objects.equals(object, other.getObject())
                && Objects.equals(subject, other.getSubject());
    }

}
