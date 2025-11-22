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
package com.apicatalog.jsonld;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * Represents a JSON-LD processing error.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#jsonlderror"> JSON-LD
 *      Error Specification</a>
 */
public final class JsonLdException extends Exception {

    private static final long serialVersionUID = -7548366490116267200L;

    private final ErrorCode code;
    
    // a node that caused the exception, if any
    //TODO private final TreeIO value;

    /**
     * Creates a new {@code JsonLdException} with the specified error code.
     *
     * @param code the error code (must not be {@code null})
     */
    public JsonLdException(ErrorCode code) {
        super(Objects.requireNonNull(code, "code must not be null").description());
        this.code = code;
    }

    /**
     * Creates a new {@code JsonLdException} with the specified error code and
     * message.
     *
     * @param code    the error code (must not be {@code null})
     * @param message the detail message
     */
    public JsonLdException(ErrorCode code, String message) {
        super(message);
        this.code = Objects.requireNonNull(code, "code must not be null");
    }

    /**
     * Creates a new {@code JsonLdException} with the specified error code and
     * cause.
     *
     * @param code  the error code (must not be {@code null})
     * @param cause the cause of the exception
     */
    public JsonLdException(ErrorCode code, Throwable cause) {
        super(Objects.requireNonNull(code, "code must not be null").description(), cause);
        this.code = code;
    }

    /**
     * Creates a new {@code JsonLdException} with the specified error code, message,
     * and cause.
     *
     * @param code    the error code (must not be {@code null})
     * @param message the detail message
     * @param cause   the cause of the exception
     */
    public JsonLdException(ErrorCode code, String message, Throwable cause) {
        super(message, cause);
        this.code = Objects.requireNonNull(code, "code must not be null");
    }

    /**
     * Returns the associated {@link ErrorCode}.
     *
     * @return the JSON-LD error code
     */
    public ErrorCode code() {
        return code;
    }

    /**
     * Returns a concise string representation optimized for logging.
     */
    @Override
    public String toString() {
        // Avoid String.format()/.formatted() overhead
        return "JsonLdException[" + code.name() + ", message=" + getMessage() + "]";
    }

    /**
     * Represents the collection of valid JSON-LD error codes.
     *
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#jsonlderrorcode">
     *      JsonLdErrorCode Specification</a>
     */
    public enum ErrorCode {

        /**
         * Two
         * <a href="https://www.w3.org/TR/rdf11-concepts/#dfn-property">properties</a>
         * which expand to the same keyword have been detected. This might occur if a
         * keyword and an alias thereof are used at the same time.
         */
        COLLIDING_KEYWORDS("Two properties which expand to the same keyword have been detected"),
        /**
         * Multiple conflicting indexes have been found for the same node.
         */
        CONFLICTING_INDEXES("Multiple conflicting indexes have been found for the same node"),

        /**
         * Maximum number of <code>@context</code> URLs exceeded.
         */
        CONTEXT_OVERFLOW("Maximum number of @context URLs exceeded"),

        /**
         * A cycle in IRI mappings has been detected.
         */
        CYCLIC_IRI_MAPPING("A cycle in IRI mappings has been detected"),

        /**
         * An <code>@id</code> entry was encountered whose value was not a string.
         */
        INVALID_KEYWORD_ID_VALUE("An @id entry was encountered whose value was not a string"),

        /**
         * An invalid value for <code>@import</code> has been found.
         */
        INVALID_KEYWORD_IMPORT_VALUE("An invalid value for @import has been found"),

        /**
         * An included block contains an invalid value.
         */
        INVALID_KEYWORD_INCLUDED_VALUE("An included block contains an invalid value"),

        /**
         * An <code>@index</code> entry was encountered whose value was not a string.
         */
        INVALID_KEYWORD_INDEX_VALUE("An @index entry was encountered whose value was not a string"),

        /**
         * An invalid value for <code>@nest</code> has been found.
         */
        INVALID_KEYWORD_NEST_VALUE("An invalid value for @nest has been found"),

        /**
         * An invalid value for <code>@prefix</code> has been found.
         */
        INVALID_KEYWORD_PREFIX_VALUE("An invalid value for @prefix has been found"),

        /**
         * An invalid value for <code>@propagate</code> has been found.
         */
        INVALID_KEYWORD_PROPAGATE_VALUE("An invalid value for @propagate has been found"),

        /**
         * An invalid value for <code>@protected</code> has been found.
         */
        INVALID_KEYWORD_PROTECTED_VALUE("An invalid value for @protected has been found"),

        /**
         * An invalid value for an <code>@reverse</code> entry has been detected, i.e.,
         * the value was not a map.
         */
        INVALID_KEYWORD_REVERSE_VALUE("An invalid value for an @reverse entry has been detected"),

        /**
         * The <code>@version</code> entry was used in a context with an out of range
         * value.
         */
        INVALID_KEYWORD_VERSION_VALUE("The @version entry was used in a context with an out of range value"),

        /**
         * The value of <code>@direction</code> is not <code>"ltr"</code>,
         * <code>"rtl"</code>, or null and thus invalid.
         */
        INVALID_BASE_DIRECTION("The value of @direction is not \"ltr\", \"rtl\", or null and thus invalid"),

        /**
         * An invalid base IRI has been detected, i.e., it is neither an IRI nor
         * <code>null</code>.
         */
        INVALID_BASE_IRI("An invalid base IRI has been detected"),

        /**
         * An <code>@container</code> entry was encountered whose value was not one of
         * the following strings: <code>@list</code>, <code>@set</code>,
         * <code>@language</code>, <code>@index</code>, <code>@id</code>,
         * <code>@graph</code>, or <code>@type</code>.
         */
        INVALID_CONTAINER_MAPPING("An @container entry was encountered whose value was not one of the following strings: @list, @set, @language, @index, @id, @graph, or @type"),

        /**
         * An entry in a context is invalid due to processing mode incompatibility.
         */
        INVALID_CONTEXT_ENTRY("An entry in a context is invalid due to processing mode incompatibility"),

        /**
         * An attempt was made to nullify a context containing protected term
         * definitions.
         */
        INVALID_CONTEXT_NULLIFICATION("An attempt was made to nullify a context containing protected term definitions"),

        /**
         * The value of the default language is not a string or <code>null</code> and
         * thus invalid.
         */
        INVALID_DEFAULT_LANGUAGE("The value of the default language is not a string or null and thus invalid"),

        /**
         * A local context contains a term that has an invalid or missing IRI mapping.
         */
        INVALID_IRI_MAPPING("A local context contains a term that has an invalid or missing IRI mapping"),

        /**
         * An invalid JSON literal was detected.
         */
        INVALID_JSON_LITERAL("An invalid JSON literal was detected"),

        /**
         * An invalid keyword alias definition has been encountered.
         */
        INVALID_KEYWORD_ALIAS("An invalid keyword alias definition has been encountered"),

        /**
         * An invalid value in a language map has been detected. It MUST be a string or
         * an array of strings.
         */
        INVALID_LANGUAGE_MAP_VALUE("An invalid value in a language map has been detected. It MUST be a string or an array of strings"),

        /**
         * An <code>@language</code> entry in a term definition was encountered whose
         * value was neither a string nor null and thus invalid.
         */
        INVALID_LANGUAGE_MAPPING("An @language entry in a term definition was encountered whose value was neither a string nor null and thus invalid"),

        /**
         * A language-tagged string with an invalid language value was detected.
         */
        INVALID_LANGUAGE_TAGGED_STRING("A language-tagged string with an invalid language value was detected"),

        /**
         * A number, <code>true</code>, or <code>false</code> with an associated
         * language tag was detected.
         */
        INVALID_LANGUAGE_TAGGED_VALUE("A number, true, or false with an associated language tag was detected"),

        /**
         * In invalid local context was detected.
         */
        INVALID_LOCAL_CONTEXT("In invalid local context was detected"),

        /**
         * No valid context document has been found for a referenced remote context.
         */
        INVALID_REMOTE_CONTEXT("No valid context document has been found for a referenced remote context"),

        /**
         * An invalid reverse property definition has been detected.
         */
        INVALID_REVERSE_PROPERTY_MAP("An invalid reverse property definition has been detected"),

        /**
         * An invalid reverse property map has been detected. No keywords apart from
         * <code>@context</code> are allowed in reverse property maps.
         */
        INVALID_REVERSE_PROPERTY_VALUE("An invalid reverse property map has been detected. No keywords apart from @context are allowed in reverse property maps"),

        /**
         * An invalid value for a reverse property has been detected. The value of an
         * inverse property must be a node object.
         */
        INVALID_REVERSE_PROPERTY("An invalid value for a reverse property has been detected. The value of an inverse property must be a node object"),

        /**
         * The local context defined within a term definition is invalid.
         */
        INVALID_SCOPED_CONTEXT("The local context defined within a term definition is invalid"),

        /**
         * A script element in HTML input which is the target of a fragment identifier
         * does not have an appropriate type attribute.
         */
        INVALID_SCRIPT_ELEMENT("A script element in HTML input which is the target of a fragment identifier does not have an appropriate type attribute"),

        /**
         * A set object or list object with disallowed entries has been detected.
         */
        INVALID_SET_OR_LIST_OBJECT("A set object or list object with disallowed entries has been detected"),

        /**
         * An invalid term definition has been detected.
         */
        INVALID_TERM_DEFINITION("An invalid term definition has been detected"),

        /**
         * An <code>@type</code> entry in a term definition was encountered whose value
         * could not be expanded to an IRI.
         */
        INVALID_TYPE_MAPPING("An @type entry in a term definition was encountered whose value could not be expanded to an IRI"),

        /**
         * An invalid value for an <code>@type</code> entry has been detected, i.e., the
         * value was neither a string nor an array of strings.
         */
        INVALID_TYPE_VALUE("An invalid value for an @type entry has been detected"),

        /**
         * A typed value with an invalid type was detected.
         */
        INVALID_TYPED_VALUE("A typed value with an invalid type was detected"),

        /**
         * An invalid value for the <code>@value</code> entry of a value object has been
         * detected, i.e., it is neither a scalar nor <code>null</code>.
         */
        INVALID_VALUE_OBJECT_VALUE("An invalid value for the @value< entry of a value object has been detected"),

        /**
         * A value object with disallowed entries has been detected.
         */
        INVALID_VALUE_OBJECT("A value object with disallowed entries has been detected"),

        /**
         * An invalid vocabulary mapping has been detected, i.e., it is neither an IRI
         * nor <code>null</code>.
         */
        INVALID_VOCAB_MAPPING("An invalid vocabulary mapping has been detected"),

        /**
         * When compacting an IRI would result in an IRI which could be confused with a
         * compact IRI (because its IRI scheme matches a term definition and it has no
         * IRI authority).
         */
        IRI_CONFUSED_WITH_PREFIX("When compacting an IRI would result in an IRI which could be confused with a compact IRI"),

        /**
         * A keyword redefinition has been detected.
         */
        KEYWORD_REDEFINITION("A keyword redefinition has been detected"),

        /**
         * The document could not be loaded or parsed as JSON.
         */
        LOADING_DOCUMENT_FAILED("The document could not be loaded or parsed"),

        /**
         * There was a problem encountered loading a remote context.
         */
        LOADING_REMOTE_CONTEXT_FAILED("There was a problem encountered loading a remote context"),

        /**
         * Multiple HTTP Link Headers [RFC8288] using the
         * http://www.w3.org/ns/json-ld#context link relation have been detected.
         */
        MULTIPLE_CONTEXT_LINK_HEADERS("Multiple HTTP Link Headers [RFC8288] using the http://www.w3.org/ns/json-ld#context link relation have been detected"),

        /**
         * An attempt was made to change the processing mode which is incompatible with
         * the previous specified version.
         */
        PROCESSING_MODE_CONFLICT("An attempt was made to change the processing mode which is incompatible with the previous specified version"),

        /**
         * An attempt was made to redefine a protected term.
         */
        PROTECTED_TERM_REDEFINITION("An attempt was made to redefine a protected term"),

        // Framing Error Codes https://www.w3.org/TR/json-ld11-framing/#error-handling

        /**
         * The frame is invalid.
         *
         * @see <a href=
         *      "https://www.w3.org/TR/json-ld11-framing/#dom-jsonldframingerrorcode-invalid-frame">invalid
         *      frame</a>
         */
        INVALID_FRAME("The frame is invalid"),

        /**
         * The value for <code>@embed</code> is not one recognized for the object embed
         * flag.
         *
         * @see <a href=
         *      "https://www.w3.org/TR/json-ld11-framing/#dom-jsonldframingerrorcode-invalid-@embed-value">invalid @embedvalue</a>
         */
        INVALID_KEYWORD_EMBED_VALUE("The value for @embed is not one recognized for the object embed flag"),

        /**
         * Experimental: JSON-LD-STAR extension.
         *
         * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
         *
         */
        INVALID_EMBEDDED_NODE("An invalid embedded node has been detected"),

        /**
         * Experimental: JSON-LD-STAR extension.
         *
         * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
         *
         */
        INVALID_ANNOTATION("An invalid annotation has been detected"),

        // Custom
        LOADING_DOCUMENT_TIMEOUT("A response has not been received within a specified time period"),

        PROCESSING_TIMEOUT_EXCEEDED("A processing has exceeded a defined timeout"),

        /**
         * An undefined term has been detected during expansion
         */
        UNDEFINED_TERM("An undefined term has been found. Set policy to ignore to pass"),

        DROPPED_NODE(""),
        
        INLINE_CONTEXT_IS_NOT_ALLOWED("Inline contexts are not allowed"),

        UNSPECIFIED("An unspecified processing error"),

        MISSING_DOCUMENT_LOADER("A document loader is not present")

        ;

        private final String description;

        private ErrorCode(String description) {
            this.description = description;
        }

        /**
         * Returns a human-readable description of this error.
         */
        public String description() {
            return description;
        }

        /**
         * Looks up a {@link ErrorCode} by its name (case-insensitive).
         *
         * @param name the name of the error code
         * @return an {@link Optional} of {@link ErrorCode}
         */
        public static Optional<ErrorCode> find(String name) {
            return Arrays.stream(values())
                    .filter(code -> code.name().equalsIgnoreCase(name))
                    .findFirst();
        }

        @Override
        public String toString() {
            return description + ", code=" + this.name() + ".";
        }
    }
}
