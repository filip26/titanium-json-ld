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
import java.util.Optional;

import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;

final class ImmutableRdfNQuad extends ImmutableRdfTriple implements RdfNQuad {

    private final RdfResource graphName;

    protected ImmutableRdfNQuad(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName) {
        super(subject, predicate, object);
        this.graphName = graphName;
    }

    @Override
    public Optional<RdfResource> getGraphName() {
        return Optional.ofNullable(graphName);
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder()
                .append(getSubject())
                .append(' ')
                .append(getPredicate())
                .append(' ')
                .append(getObject())
                .append(' ');

        if (graphName != null) {
            builder.append(graphName).append(' ');
        }

        return builder.append('.').toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(graphName);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() == obj.getClass()) {
            ImmutableRdfNQuad other = (ImmutableRdfNQuad) obj;
            return Objects.equals(graphName, other.graphName);
        }
        if (!(obj instanceof RdfNQuad)) {
            return false;
        }
        RdfNQuad other = (RdfNQuad) obj;
        return Objects.equals(graphName, other.getGraphName().orElse(null));
    }

}
