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
package com.apicatalog.jsonld.json;

import jakarta.json.JsonValue;

/**
 * @deprecated since 1.6.0, use titanium-jcs and
 *             {@link com.apicatalog.jcs.Jcs#canonize(JsonValue)}.
 * 
 * @see <a href=
 *      "https://tools.ietf.org/html/draft-rundgren-json-canonicalization-scheme-17">JSON
 *      Canonicalization Scheme (JCS)</a>
 *
 */
public final class JsonCanonicalizer {
    public static final String canonicalize(final JsonValue value) {
        return com.apicatalog.jcs.Jcs.canonize(value);
    }
}
