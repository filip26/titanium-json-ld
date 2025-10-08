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
package com.apicatalog.jsonld.expansion;

import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.node.BlankNode;
import com.apicatalog.jsonld.uri.UriResolver;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 * Implements the
 * <a href="https://www.w3.org/TR/json-ld11-api/#iri-expansion">IRI
 * Expansion</a> algorithm from the JSON-LD 1.1 API specification.
 *
 * <p>
 * This class encapsulates the state and logic for a single, configurable
 * expansion operation. It is used by creating an instance with an active
 * context via {@link #with(Context)}, setting flags such as {@code vocab} or
 * {@code documentRelative} through its fluent API, and then calling the
 * {@link #expand(String)} method to perform the expansion.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 * 
 * <pre>{@code
 * String expandedIri = UriExpansion
 *         .with(activeContext)
 *         .vocab(true)
 *         .expand("myTerm");
 * }</pre>
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#iri-expansion">IRI
 *      Expansion Algorithm</a>
 */
public final class UriExpansion {

    private static final Logger LOGGER = Logger.getLogger(UriExpansion.class.getName());

    // mandatory
    private final Context activeContext;

    // optional
    private boolean documentRelative;
    private boolean vocab;
    private UriValidationPolicy uriValidation;

    private JsonObject localContext;
    private Map<String, Boolean> defined;

    private UriExpansion(final Context activeContext) {
        this.activeContext = activeContext;

        // default values
        this.documentRelative = false;
        this.vocab = false;
        this.localContext = null;
        this.defined = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    /**
     * Creates a new {@link UriExpansion} instance for the given active context.
     * This is the entry point for configuring and executing an IRI expansion.
     *
     * @param activeContext the active context to use for expansion
     * @return a new, configurable {@code UriExpansion} instance
     */
    public static final UriExpansion with(final Context activeContext) {
        return new UriExpansion(activeContext);
    }

    /**
     * Performs IRI expansion on a string value according to the configured
     * parameters.
     *
     * @param value the string to expand (e.g., a term, compact IRI, or absolute
     *              IRI)
     * @return the fully expanded IRI as a {@link String}, or {@code null} if the
     *         value has an invalid keyword-like form
     * @throws JsonLdError if an error occurs during context processing
     */
    public String expand(final String value) throws JsonLdError {

        // 1. If value is a keyword or null, return value as is.
        if (value == null || Keywords.contains(value)) {
            return value;
        }

        // 2. If value has the form of a keyword (i.e., it matches the ABNF rule
        // "@"1*ALPHA from [RFC5234]),
        // a processor SHOULD generate a warning and return null.
        if (Keywords.matchForm(value)) {
            LOGGER.log(Level.WARNING, "Value [{0}] of keyword form [@1*ALPHA] is not allowed.", value);
            return null;
        }

        initLocalContext(value);

        final Optional<TermDefinition> definition = activeContext.getTerm(value)
                .filter(term -> vocab || Keywords.contains(term.getUriMapping()));

        // 4. if active context has a term definition for value,
        // and the associated IRI mapping is a keyword, return that keyword.
        // 5. If vocab is true and the active context has a term definition for value,
        // return the associated IRI mapping
        if (definition.isPresent()) {
            return definition.get().getUriMapping();
        }

        String result = value;

        // 6. If value contains a colon (:) anywhere after the first character, it is
        // a compact IRI, or a blank node identifier
        final int splitIndex = result.indexOf(':', 1);

        if (splitIndex != -1) {
            // 6.1. Split value into a prefix and suffix
            // at the first occurrence of a colon (:).
            // 6.2. If prefix is underscore (_) or suffix begins with double-forward-slash
            // (//), return value as it is already an IRI or a blank node identifier.
            if ((splitIndex == 1 && result.charAt(0) == '_')
                    || (((splitIndex + 2) < result.length())
                            && result.charAt(splitIndex + 1) == '/'
                            && result.charAt(splitIndex + 2) == '/')) {
                return result;
            }

            result = initPropertyContext(
                    result.substring(0, splitIndex),
                    (splitIndex + 1) < result.length()
                            ? result.substring(splitIndex + 1)
                            : "",
                    result);

            // 6.5
            if (BlankNode.hasPrefix(result) || UriUtils.isAbsoluteUri(result, uriValidation)) {
                return result;
            }
        }

        return expandResult(result);
    }

    /**
     * Sets the {@code documentRelative} flag. If {@code true}, relative IRIs are
     * resolved against the document's base URI.
     *
     * @param value the new value for the {@code documentRelative} flag
     * @return this instance, for method chaining
     */
    public UriExpansion documentRelative(boolean value) {
        this.documentRelative = value;
        return this;
    }

    /**
     * Sets the {@code vocab} flag. If {@code true}, terms are expanded using the
     * active context's vocabulary mapping.
     *
     * @param value the new value for the {@code vocab} flag
     * @return this instance, for method chaining
     */
    public UriExpansion vocab(boolean value) {
        this.vocab = value;
        return this;
    }

    /**
     * Sets the local context to be used during expansion.
     *
     * @param value the local context as a {@link JsonObject}
     * @return this instance, for method chaining
     */
    public UriExpansion localContext(JsonObject value) {
        this.localContext = value;
        return this;
    }

    /**
     * Sets the map of defined terms to prevent circular definitions during context
     * processing.
     *
     * @param value a map tracking which terms have been defined
     * @return this instance, for method chaining
     */
    public UriExpansion defined(Map<String, Boolean> value) {
        this.defined = value;
        return this;
    }

    /**
     * Sets the URI validation policy for checking absolute URIs.
     *
     * @param uriValidation the policy to apply
     * @return this instance, for method chaining
     */
    public UriExpansion uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    /**
     * Implements step 3 of the IRI Expansion algorithm. If a local context exists,
     * this method ensures a term definition is created for the given value if it's
     * defined in that context.
     *
     * @param value the term to potentially define
     * @throws JsonLdError if an error occurs during term creation
     */
    private void initLocalContext(final String value) throws JsonLdError {
        /*
         * 3. If local context is not null, it contains an entry with a key that equals
         * value, and the value of the entry for value in defined is not true, invoke
         * the Create Term Definition algorithm, passing active context, local context,
         * value as term, and defined. This will ensure that a term definition is
         * created for value in active context during Context Processing
         */
        if (localContext != null && localContext.containsKey(value)) {

            final JsonValue entryValue = localContext.get(value);

            if (JsonUtils.isString(entryValue)) {

                final String entryValueString = ((JsonString) entryValue).getString();

                if (!defined.containsKey(entryValueString) || Boolean.FALSE.equals(defined.get(entryValueString))) {
                    activeContext.newTerm(localContext, defined).create(value);
                }
            }
        }
    }

    /**
     * Implements steps 6.3 and 6.4 of the IRI Expansion algorithm. This method
     * handles the expansion of a potential compact IRI (prefix:suffix).
     *
     * @param prefix the prefix part of the compact IRI
     * @param suffix the suffix part of the compact IRI
     * @param result the original value, to be returned if expansion fails
     * @return the expanded IRI or the original result
     * @throws JsonLdError if an error occurs during term creation
     */
    private String initPropertyContext(final String prefix, final String suffix, final String result) throws JsonLdError {

        // 6.3. Create term definition for the prefix if it exists in the local context.
        if (localContext != null && localContext.containsKey(prefix) && !Boolean.TRUE.equals(defined.get(prefix))) {
            activeContext.newTerm(localContext, defined).create(prefix);
        }

        // 6.4. If the prefix is a term in the active context, append the suffix to its
        // IRI mapping.
        return activeContext.getTerm(prefix)
                .filter(TermDefinition::isPrefix)
                .map(TermDefinition::getUriMapping)
                .map(uriMapping -> uriMapping.concat(suffix))
                .orElse(result);
    }

    /**
     * Implements steps 7-9 of the IRI Expansion algorithm. Applies vocabulary
     * mapping or document-relative resolution as a final step.
     *
     * @param result the IRI to be finalized
     * @return the finalized, expanded IRI
     */
    private String expandResult(final String result) {
        // 7. If vocab is true, and active context has a vocabulary mapping,
        // return the result of concatenating the vocabulary mapping with value.
        if (vocab && activeContext.getVocabularyMapping() != null) {
            return activeContext.getVocabularyMapping().concat(result);

            // 8. If documentRelative is true, resolve the result against the base URI.
        } else if (documentRelative) {
            return UriResolver.resolve(activeContext.getBaseUri(), result);
        }

        // 9. Return the result as is.
        return result;
    }
}
