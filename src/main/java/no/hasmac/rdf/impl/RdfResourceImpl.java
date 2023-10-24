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

import java.util.Objects;

import no.hasmac.rdf.RdfResource;

final class RdfResourceImpl implements RdfResource {

    private final String value;
    private final boolean blankNode;

    protected RdfResourceImpl(final String value, boolean isBlankNode) {
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
        if (getClass() != obj.getClass()) {
            return false;
        }
        RdfResourceImpl other = (RdfResourceImpl) obj;
        return Objects.equals(value, other.value);
    }

    @Override
    public String toString() {
        return Objects.toString(value);
    }
}
