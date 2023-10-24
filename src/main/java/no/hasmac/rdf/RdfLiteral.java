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
package no.hasmac.rdf;

import java.util.Optional;

/**
 * The {@link RdfLiteral} interface describes an immutable <code>RDF Literal</code>.
 */
public interface RdfLiteral extends RdfValue {

    /**
     * Get the lexical value of the literal.
     *
     * @return lexical value, never <code>null</code>
     */
    @Override
    String getValue();

    /**
     * An absolute IRI denoting the datatype IRI of the literal. If the value is
     * rdf:langString, {@link #getLanguage()} value is present.
     *
     * @return an absolute IRI, never <code>null</code>
     */
    String getDatatype();

    /**
     * An optional language tag. If this value is specified, {@link #getDatatype()} returns rdf:langString.
     *
     * @return language tag or {@link Optional#empty()} if not set
     */
    Optional<String> getLanguage();

    @Override
    default boolean isIRI() {
        return false;
    }

    @Override
    default boolean isBlankNode() {
        return false;
    }

    @Override
    default RdfLiteral asLiteral() {
        return this;
    }
}
