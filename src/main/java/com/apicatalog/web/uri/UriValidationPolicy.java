/*
 * Copyright 2020-2026 the original author or authors.
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
package com.apicatalog.web.uri;

/**
 * Represents different URI validation policies. These policies determine the
 * extent to which URIs are validated during processing.
 */
public enum UriValidationPolicy {

    /**
     * Disables URI validation entirely, accepting all input as-is. This option
     * provides maximum flexibility but may lead to non-compliant or invalid URI
     * handling.
     */
    None,

    /**
     * Validates only the scheme component of the URI, allowing for more lenient
     * processing while still ensuring a recognizable scheme (e.g., "https:",
     * "did:", "file:", ...).
     */
    SchemeOnly,

    /**
     * Performs full URI validation, ensuring that the URI conforms to all syntax
     * and structural requirements defined by relevant standards.
     */
    Full;
}
