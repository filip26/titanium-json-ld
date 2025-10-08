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
package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.processor.ProcessingRuntime;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 *
 */
public interface Context {

    Optional<TermDefinition> getTerm(final String value);

    DirectionType getDefaultBaseDirection();

    String getDefaultLanguage();

    URI getBaseUri();

    String getVocabularyMapping();

    Context getPreviousContext();

    ProcessingRuntime runtime();

    TermDefinitionBuilder newTerm(JsonObject localContext, Map<String, Boolean> defined);

    ActiveContextBuilder newContext();

    // ---

    UriExpansion uriExpansion();

    Map<String, ?> expandValue(final String activeProperty, final JsonValue value) throws JsonLdError;
    // ---
//  void createInverseContext();

//  boolean containsTerm(final String term);

//  boolean containsProtectedTerm();

//    URI getBaseUrl();

//    InverseContext getInverseContext();

//    Map<String, TermDefinition> getTermsMapping();

//    Collection<String> getTerms();

//    UriCompaction uriCompaction();

//    ValueCompaction valueCompaction();

//    TermSelector termSelector(final String variable, final Collection<String> containerMapping, final String typeLanguage);

    // ---
}
