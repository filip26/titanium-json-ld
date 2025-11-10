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

import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#term-selection">Term
 *      Selection</a>
 *
 */
final class TermSelector {

    public static Optional<String> match(
            final Collection<String> preferredValues,
            final ActiveContext activeContext,
            final String variable,
            final Collection<String> containers,
            final String typeLanguage) {

        // 1. If the active context has a null inverse context,
        // set inverse context in active context to the result of calling
        // the Inverse Context Creation algorithm using active context.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        // 2. Initialize inverse context to the value of inverse context in active
        // context.
        final InverseContext inverseContext = activeContext.getInverseContext();

        // 4.
        return containers
                .stream()
                .filter(container -> inverseContext.contains(variable, container, typeLanguage))
                .flatMap(container -> preferredValues
                        .stream()
                        .map(item -> inverseContext.get(variable, container, typeLanguage, item)))
                .flatMap(o -> o.map(Stream::of).orElseGet(Stream::empty))
                .findFirst();
    }
}