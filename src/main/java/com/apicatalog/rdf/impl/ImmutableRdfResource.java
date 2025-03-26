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
final class ImmutableRdfResource implements RdfResource {

    private final String value;
    private final boolean blankNode;

    protected ImmutableRdfResource(final String value, boolean isBlankNode) {
        this.value = value;
        this.blankNode = isBlankNode;
    }

    @Override
    public boolean isBlankNode() {
        return blankNode;
    }

    @Override
    public boolean isIRI() {
        return !blankNode;
    }

    @Override
    public String getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
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
            ImmutableRdfResource other = (ImmutableRdfResource) obj;
            return Objects.equals(value, other.value);
        }        
        if (!(obj instanceof RdfResource)) {
            return false;
        }
        RdfResource other = (RdfResource) obj;
        return Objects.equals(value, other.getValue());
    }

    @Override
    public String toString() {
        if (blankNode) {
            return Objects.toString(value);
        }
        return '<' + Objects.toString(value) + '>';
    }
}
