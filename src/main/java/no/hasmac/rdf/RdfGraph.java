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

import java.util.List;

import no.hasmac.jsonld.JsonLd;


/**
 * The {@link RdfGraph} interface describes operations on an RDF graph used by
 * the @link {@link JsonLd#fromRdf} and
 * {@link JsonLd#toRdf(java.net.URI)} methods in the
 * {@link JsonLd} interface. The interface may be used for constructing
 * a new {@link RdfGraph}, which is composed of zero or more {@link RdfTriple}
 * instances.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-140206580">RdfGraph
 *      IDL</a>
 *
 */
public interface RdfGraph {

    boolean contains(RdfTriple triple);

    List<RdfTriple> toList();

}
