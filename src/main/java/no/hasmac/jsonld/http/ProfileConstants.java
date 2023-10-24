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
package no.hasmac.jsonld.http;

/**
 * @see <a href="https://www.w3.org/TR/json-ld11/#iana-considerations">IANA Considerations</a>
 */
public final class ProfileConstants {

    public static final String EXPANDED = "http://www.w3.org/ns/json-ld#expanded";

    public static final String COMPACTED = "http://www.w3.org/ns/json-ld#compacted";

    public static final String CONTEXT = "http://www.w3.org/ns/json-ld#context";

    public static final String FLATTENED = "http://www.w3.org/ns/json-ld#flattened";

    public static final String FRAME = "http://www.w3.org/ns/json-ld#frame";

    public static final String FRAMED = "http://www.w3.org/ns/json-ld#framed";

    private ProfileConstants() {
    }
}
