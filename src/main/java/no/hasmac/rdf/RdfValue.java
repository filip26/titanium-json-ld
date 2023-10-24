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

/**
 * An immutable RDF statement's value. Represents an absolute IRI or blank node identifier or RDF literal.
 *
 * @since 0.8.4
 */
public interface RdfValue {

    /**
     * Indicates if the value type is RDF literal.
     *
     * @return <code>true</code> if the value type is literal, <code>false</code> otherwise.
     */
    default boolean isLiteral() {
        return false;
    }

    /**
     * Indicates if the value type is an absolute IRI.
     *
     * @return <code>true</code> if the value type is IRI, <code>false</code> otherwise.
     */
    default boolean isIRI() {
        return false;
    }

    /**
     * Indicates if the value type is blank node identifier.
     *
     * @return <code>true</code> if the value type is blank node, <code>false</code> otherwise.
     */
    default boolean isBlankNode() {
        return false;
    }

    /**
     * Return the RdfValue as a RdfLiteral
     *
     * @return the RdfValue as a RdfLiteral
     * @throws ClassCastException if the RdfValue is not a RdfLiteral
     *
     */
    RdfLiteral asLiteral();

    /**
     * Returns raw {@link String} representation of the value.
     *
     * @return  text representing the value.
     */
    String getValue();


    @Override
    boolean equals(Object o);

    @Override
    int hashCode();

    @Override
    String toString();
}
