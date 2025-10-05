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
package com.apicatalog.jsonld.api;

import com.apicatalog.jsonld.loader.JsonLdLoader;

public interface LoaderApi<R> {

    /**
     * Set the loader to be used to retrieve remote documents and
     * contexts, implementing the {@link JsonLdLoader}. If specified, it is
     * used to retrieve remote documents and contexts; otherwise, if not specified,
     * the processor's built-in loader is used.
     *
     * @param loader the {@link JsonLdLoader} instance, never {@code null}.
     * @return builder instance
     */
    R loader(JsonLdLoader loader);

}
