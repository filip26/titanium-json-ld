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
package no.hasmac.jsonld.api;

import java.net.URI;

import no.hasmac.jsonld.document.Document;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;

public interface ContextApi<R> {

    /**
     * A context that is used to initialize the active context.
     *
     * @param contextUri {@link URI} referring to a context
     * @return builder instance
     */
    R context(URI contextUri);

    /**
     * A context that is used to initialize the active context.
     *
     * @param contextLocation <code>IRI</code> referring to a context
     * @return builder instance
     */
    R context(String contextLocation);

    /**
     * A context that is used to initialize the active context.
     *
     * @param context {@link JsonObject}, a sequence of {@link JsonObject}, or a {@link JsonString} representing an <code>IRI</code>
     * @return builder instance
     */
    R context(JsonStructure context);

    /**
     * A context that is used to initialize the active context.
     *
     * @param context {@link Document} representing a context
     * @return builder instance
     */
    R context(Document context);
}
