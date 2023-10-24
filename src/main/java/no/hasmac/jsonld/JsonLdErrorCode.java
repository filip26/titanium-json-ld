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

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

/**
 * The {@link JsonLdErrorCode} represents the collection of valid JSON-LD error
 * codes.
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#jsonlderrorcode">JsonLdErrorCode
 *      Specification</a>
 *
 */
public enum JsonLdErrorCode {

    /**
     * Two
     * <a href="https://www.w3.org/TR/rdf11-concepts/#dfn-property">properties</a>
     * which expand to the same keyword have been detected. This might occur if a
     * keyword and an alias thereof are used at the same time.
     */
    COLLIDING_KEYWORDS,
    /**
     * Multiple conflicting indexes have been found for the same node.
     */
    CONFLICTING_INDEXES,
    /**
     * Maximum number of <code>@context</code> URLs exceeded.
     */
    CONTEXT_OVERFLOW,
    /**
     * A cycle in IRI mappings has been detected.
     */
    CYCLIC_IRI_MAPPING,
    /**
     * An <code>@id</code> entry was encountered whose value was not a string.
     */
    INVALID_KEYWORD_ID_VALUE,
    /**
     * An invalid value for <code>@import</code> has been found.
     */
    INVALID_KEYWORD_IMPORT_VALUE,
    /**
     * An included block contains an invalid value.
     */
    INVALID_KEYWORD_INCLUDED_VALUE,
    /**
     * An <code>@index</code> entry was encountered whose value was not a string.
     */
    INVALID_KEYWORD_INDEX_VALUE,
    /**
     * An invalid value for <code>@nest</code> has been found.
     */
    INVALID_KEYWORD_NEST_VALUE,
    /**
     * An invalid value for <code>@prefix</code> has been found.
     */
    INVALID_KEYWORD_PREFIX_VALUE,
    /**
     * An invalid value for <code>@propagate</code> has been found.
     */
    INVALID_KEYWORD_PROPAGATE_VALUE,
    /**
     * An invalid value for <code>@protected</code> has been found.
     */
    INVALID_KEYWORD_PROTECTED_VALUE,
    /**
     * An invalid value for an <code>@reverse</code> entry has been detected, i.e.,
     * the value was not a map.
     */
    INVALID_KEYWORD_REVERSE_VALUE,
    /**
     * The <code>@version</code> entry was used in a context with an out of range
     * value.
     */
    INVALID_KEYWORD_VERSION_VALUE,
    /**
     * The value of <code>@direction</code> is not <code>"ltr"</code>,
     * <code>"rtl"</code>, or null and thus invalid.
     */
    INVALID_BASE_DIRECTION,
    /**
     * An invalid base IRI has been detected, i.e., it is neither an IRI nor
     * <code>null</code>.
     */
    INVALID_BASE_IRI,
    /**
     * An <code>@container</code> entry was encountered whose value was not one of
     * the following strings: <code>@list</code>, <code>@set</code>,
     * <code>@language</code>, <code>@index</code>, <code>@id</code>,
     * <code>@graph</code>, or <code>@type</code>.
     */
    INVALID_CONTAINER_MAPPING,
    /**
     * An entry in a context is invalid due to processing mode incompatibility.
     */
    INVALID_CONTEXT_ENTRY,
    /**
     * An attempt was made to nullify a context containing protected term
     * definitions.
     */
    INVALID_CONTEXT_NULLIFICATION,

    /**
     * The value of the default language is not a string or <code>null</code> and
     * thus invalid.
     */
    INVALID_DEFAULT_LANGUAGE,
    /**
     * A local context contains a term that has an invalid or missing IRI mapping.
     */
    INVALID_IRI_MAPPING,
    /**
     * An invalid JSON literal was detected.
     */
    INVALID_JSON_LITERAL,
    /**
     * An invalid keyword alias definition has been encountered.
     */
    INVALID_KEYWORD_ALIAS,
    /**
     * An invalid value in a language map has been detected. It MUST be a string or
     * an array of strings.
     */
    INVALID_LANGUAGE_MAP_VALUE,
    /**
     * An <code>@language</code> entry in a term definition was encountered whose
     * value was neither a string nor null and thus invalid.
     */
    INVALID_LANGUAGE_MAPPING,
    /**
     * A language-tagged string with an invalid language value was detected.
     */
    INVALID_LANGUAGE_TAGGED_STRING,
    /**
     * A number, <code>true</code>, or <code>false</code> with an associated
     * language tag was detected.
     */
    INVALID_LANGUAGE_TAGGED_VALUE,
    /**
     * In invalid local context was detected.
     */
    INVALID_LOCAL_CONTEXT,
    /**
     * No valid context document has been found for a referenced remote context.
     */
    INVALID_REMOTE_CONTEXT,
    /**
     * An invalid reverse property definition has been detected.
     */
    INVALID_REVERSE_PROPERTY_MAP,
    /**
     * An invalid reverse property map has been detected. No keywords apart from
     * <code>@context</code> are allowed in reverse property maps.
     */
    INVALID_REVERSE_PROPERTY_VALUE,
    /**
     * An invalid value for a reverse property has been detected. The value of an
     * inverse property must be a node object.
     */
    INVALID_REVERSE_PROPERTY,
    /**
     * The local context defined within a term definition is invalid.
     */
    INVALID_SCOPED_CONTEXT,
    /**
     * A script element in HTML input which is the target of a fragment identifier
     * does not have an appropriate type attribute.
     */
    INVALID_SCRIPT_ELEMENT,
    /**
     * A set object or list object with disallowed entries has been detected.
     */
    INVALID_SET_OR_LIST_OBJECT,
    /**
     * An invalid term definition has been detected.
     */
    INVALID_TERM_DEFINITION,
    /**
     * An <code>@type</code> entry in a term definition was encountered whose value
     * could not be expanded to an IRI.
     */
    INVALID_TYPE_MAPPING,
    /**
     * An invalid value for an <code>@type</code> entry has been detected, i.e., the
     * value was neither a string nor an array of strings.
     */
    INVALID_TYPE_VALUE,
    /**
     * A typed value with an invalid type was detected.
     */
    INVALID_TYPED_VALUE,

    /**
     * An invalid value for the <code>@value</code> entry of a value object has been
     * detected, i.e., it is neither a scalar nor <code>null</code>.
     */
    INVALID_VALUE_OBJECT_VALUE,
    /**
     * A value object with disallowed entries has been detected.
     */
    INVALID_VALUE_OBJECT,
    /**
     * An invalid vocabulary mapping has been detected, i.e., it is neither an IRI
     * nor <code>null</code>.
     */
    INVALID_VOCAB_MAPPING,
    /**
     * When compacting an IRI would result in an IRI which could be confused with a
     * compact IRI (because its IRI scheme matches a term definition and it has no
     * IRI authority).
     */
    IRI_CONFUSED_WITH_PREFIX,
    /**
     * A keyword redefinition has been detected.
     */
    KEYWORD_REDEFINITION,
    /**
     * The document could not be loaded or parsed as JSON.
     */
    LOADING_DOCUMENT_FAILED,
    /**
     * There was a problem encountered loading a remote context.
     */
    LOADING_REMOTE_CONTEXT_FAILED,
    /**
     * Multiple HTTP Link Headers [RFC8288] using the
     * http://www.w3.org/ns/json-ld#context link relation have been detected.
     */
    MULTIPLE_CONTEXT_LINK_HEADERS,
    /**
     * An attempt was made to change the processing mode which is incompatible with
     * the previous specified version.
     */
    PROCESSING_MODE_CONFLICT,
    /**
     * An attempt was made to redefine a protected term.
     */
    PROTECTED_TERM_REDEFINITION,


    // Framing Error Codes https://www.w3.org/TR/json-ld11-framing/#error-handling

    /**
     * The frame is invalid.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11-framing/#dom-jsonldframingerrorcode-invalid-frame">invalid frame</a>
     */
    INVALID_FRAME,

    /**
     * The value for <code>@embed</code> is not one recognized for the object embed flag.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11-framing/#dom-jsonldframingerrorcode-invalid-@embed-value">invalid @embed value</a>
     */
    INVALID_KEYWORD_EMBED_VALUE,

    /**
     * Experimental: JSON-LD-STAR extension.
     *
     * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
     *
     */
    INVALID_EMBEDDED_NODE,

    /**
     * Experimental: JSON-LD-STAR extension.
     *
     * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
     *
     */
    INVALID_ANNOTATION,

    // Custom
    UNSPECIFIED;

    private static final Map<JsonLdErrorCode, String> CODE_TO_MESSAGE;

    static {

        final Map<JsonLdErrorCode, String> messages = new EnumMap<>(JsonLdErrorCode.class);

        messages.put(COLLIDING_KEYWORDS, "Two properties which expand to the same keyword have been detected");
        messages.put(CONFLICTING_INDEXES, "Multiple conflicting indexes have been found for the same node");
        messages.put(CONTEXT_OVERFLOW, "Maximum number of @context URLs exceeded");
        messages.put(CYCLIC_IRI_MAPPING, "A cycle in IRI mappings has been detected");
        messages.put(INVALID_KEYWORD_ID_VALUE, "An @id entry was encountered whose value was not a string");
        messages.put(INVALID_KEYWORD_IMPORT_VALUE, "An invalid value for @import has been found");
        messages.put(INVALID_KEYWORD_INCLUDED_VALUE, "An included block contains an invalid value");
        messages.put(INVALID_KEYWORD_INDEX_VALUE, "An @index entry was encountered whose value was not a string");
        messages.put(INVALID_KEYWORD_NEST_VALUE, "An invalid value for @nest has been found");
        messages.put(INVALID_KEYWORD_PREFIX_VALUE, "An invalid value for @prefix has been found");
        messages.put(INVALID_KEYWORD_PROPAGATE_VALUE, "An invalid value for @propagate has been found");
        messages.put(INVALID_KEYWORD_PROTECTED_VALUE, "An invalid value for @protected has been found");
        messages.put(INVALID_KEYWORD_REVERSE_VALUE, "An invalid value for an @reverse entry has been detected");
        messages.put(INVALID_KEYWORD_VERSION_VALUE, "The @version entry was used in a context with an out of range value");
        messages.put(INVALID_BASE_DIRECTION, "The value of @direction is not \"ltr\", \"rtl\", or null and thus invalid");
        messages.put(INVALID_BASE_IRI, "An invalid base IRI has been detected");
        messages.put(INVALID_CONTAINER_MAPPING, "An @container entry was encountered whose value was not one of the following strings: @list, @set, @language, @index, @id, @graph, or @type");
        messages.put(INVALID_CONTEXT_ENTRY, "An entry in a context is invalid due to processing mode incompatibility");
        messages.put(INVALID_CONTEXT_NULLIFICATION, "An attempt was made to nullify a context containing protected term definitions");
        messages.put(INVALID_DEFAULT_LANGUAGE, "The value of the default language is not a string or null and thus invalid");
        messages.put(INVALID_IRI_MAPPING, "A local context contains a term that has an invalid or missing IRI mapping");
        messages.put(INVALID_JSON_LITERAL, "An invalid JSON literal was detected");
        messages.put(INVALID_KEYWORD_ALIAS, "An invalid keyword alias definition has been encountered");
        messages.put(INVALID_LANGUAGE_MAP_VALUE, "An invalid value in a language map has been detected. It MUST be a string or an array of strings");
        messages.put(INVALID_LANGUAGE_MAPPING, "An @language entry in a term definition was encountered whose value was neither a string nor null and thus invalid");
        messages.put(INVALID_LANGUAGE_TAGGED_STRING, "A language-tagged string with an invalid language value was detected");
        messages.put(INVALID_LANGUAGE_TAGGED_VALUE, "A number, true, or false with an associated language tag was detected");
        messages.put(INVALID_LOCAL_CONTEXT, "In invalid local context was detected");
        messages.put(INVALID_REMOTE_CONTEXT, "No valid context document has been found for a referenced remote context");
        messages.put(INVALID_REVERSE_PROPERTY_MAP, "An invalid reverse property definition has been detected");
        messages.put(INVALID_REVERSE_PROPERTY_VALUE, "An invalid reverse property map has been detected. No keywords apart from @context are allowed in reverse property maps");
        messages.put(INVALID_REVERSE_PROPERTY, "An invalid value for a reverse property has been detected. The value of an inverse property must be a node object");
        messages.put(INVALID_SCOPED_CONTEXT, "The local context defined within a term definition is invalid");
        messages.put(INVALID_SCRIPT_ELEMENT, "A script element in HTML input which is the target of a fragment identifier does not have an appropriate type attribute");
        messages.put(INVALID_SET_OR_LIST_OBJECT, "A set object or list object with disallowed entries has been detected");
        messages.put(INVALID_TERM_DEFINITION, "An invalid term definition has been detected");
        messages.put(INVALID_TYPE_MAPPING, "An @type entry in a term definition was encountered whose value could not be expanded to an IRI");
        messages.put(INVALID_TYPE_VALUE, "An invalid value for an @type entry has been detected");
        messages.put(INVALID_TYPED_VALUE, "A typed value with an invalid type was detected");
        messages.put(INVALID_VALUE_OBJECT_VALUE, "An invalid value for the @value< entry of a value object has been detected");
        messages.put(INVALID_VALUE_OBJECT, "A value object with disallowed entries has been detected");
        messages.put(INVALID_VOCAB_MAPPING, "An invalid vocabulary mapping has been detected");
        messages.put(IRI_CONFUSED_WITH_PREFIX, "When compacting an IRI would result in an IRI which could be confused with a compact IRI");
        messages.put(KEYWORD_REDEFINITION, "A keyword redefinition has been detected");
        messages.put(LOADING_DOCUMENT_FAILED, "The document could not be loaded or parsed");
        messages.put(LOADING_REMOTE_CONTEXT_FAILED, "There was a problem encountered loading a remote context");
        messages.put(MULTIPLE_CONTEXT_LINK_HEADERS, "Multiple HTTP Link Headers [RFC8288] using the http://www.w3.org/ns/json-ld#context link relation have been detected");
        messages.put(PROCESSING_MODE_CONFLICT, "An attempt was made to change the processing mode which is incompatible with the previous specified version");
        messages.put(PROTECTED_TERM_REDEFINITION, "An attempt was made to redefine a protected term");
        messages.put(INVALID_FRAME, "The frame is invalid");
        messages.put(INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag");
        messages.put(INVALID_EMBEDDED_NODE, "An invalid embedded node has been detected.");
        messages.put(INVALID_ANNOTATION, "An invalid annotation has been detected");

        CODE_TO_MESSAGE = Collections.unmodifiableMap(messages);
    }

    public String toMessage() {
        return CODE_TO_MESSAGE.getOrDefault(this, "Processing error") + " [code=" + this + "].";
    }
}

