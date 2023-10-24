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
 * The {@link RdfTriple} interface describes an immutable RDF triple.
 */
public interface RdfTriple {

    /**
     * An absolute IRI or blank node identifier denoting the subject of the triple.
     *
     * @return an absolute URI or blank node
     */
    RdfResource getSubject();

    /**
     * An absolute IRI or blank node identifier denoting the predicate of the triple.
     *
     * @return an absolute URI or blank node
     */
    RdfResource getPredicate();


    /**
     * An absolute IRI or blank node identifier or {@link RdfLiteral}.
     *
     * @return {@link RdfValue}
     */
    RdfValue getObject();

}
