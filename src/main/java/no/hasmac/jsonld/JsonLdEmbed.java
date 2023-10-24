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
package no.hasmac.jsonld;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-framing/#dom-jsonldembed">JsonLdEmbed</a>
 *
 */
public enum JsonLdEmbed {

    /**
     * Always embed node objects as property values,
     * unless this would cause a circular reference.
     */
    ALWAYS,

    /**
     * Always use a node reference when serializing matching values.
     */
    NEVER,

    /**
     * Only a single value within a given node object should be embedded,
     * other values of other properties use a node reference.
     * This is the default value if neither @embed nor object embed flag is specified.
     */
    ONCE

}
