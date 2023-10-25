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
package no.hasmac.jsonld.expansion;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.BlankNode;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.uri.UriResolver;
import no.hasmac.jsonld.uri.UriUtils;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#algorithm-4">IRI
 * Expansion</a>
 */
public final class UriExpansion {

    private static final Logger LOGGER = Logger.getLogger(UriExpansion.class.getName());

    // mandatory
    private final ActiveContext activeContext;

    // optional
    private boolean documentRelative;
    private boolean vocab;
    private boolean uriValidation;

    private JsonObject localContext;
    private Map<String, Boolean> defined;

    private UriExpansion(final ActiveContext activeContext) {
        this.activeContext = activeContext;

        // default values
        this.documentRelative = false;
        this.vocab = false;
        this.localContext = null;
        this.defined = null;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static UriExpansion with(final ActiveContext activeContext) {
        return new UriExpansion(activeContext);
    }

    public UriExpansion documentRelative(boolean value) {
        this.documentRelative = value;
        return this;
    }

    public UriExpansion vocab(boolean value) {
        this.vocab = value;
        return this;
    }

    public UriExpansion localContext(JsonObject value) {
        this.localContext = value;
        return this;
    }

    public UriExpansion defined(Map<String, Boolean> value) {
        this.defined = value;
        return this;
    }

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

        TermDefinition definition = activeContext.getTermNullable(value);

        // 4. if active context has a term definition for value,
        // and the associated IRI mapping is a keyword, return that keyword.
        // 5. If vocab is true and the active context has a term definition for value,
        // return the associated IRI mapping
        if (definition != null && (Keywords.contains(definition.getUriMapping()) || vocab)) {
            return definition.getUriMapping();
        }

        String result = value;

        // 6. If value contains a colon (:) anywhere after the first character, it is
        // either an IRI,
        // a compact IRI, or a blank node identifier
        int indexOfColon = result.indexOf(':', 1);
        if (indexOfColon != -1) {

            // 6.2. If prefix is underscore (_)
            // return value as it is a blank node identifier.
            if (indexOfColon == 1 && result.charAt(0) == '_') {
                return result;
            }


            // 6.2. If suffix begins with double-forward-slash
            // (//),
            // return value as it is already an IRI or a blank node identifier.
            if (result.length() > indexOfColon + 2 && result.charAt(indexOfColon + 1) == '/' && result.charAt(indexOfColon + 2) == '/') {
                return result;
            }

            // 6.1. Split value into a prefix and suffix at the first occurrence of a colon
            // (:).
            String[] split = result.split(":", 2);

            result = initPropertyContext(split[0], split[1], result);

            // 6.5
            if (BlankNode.hasPrefix(result) || UriUtils.isAbsoluteUri(result, uriValidation)) {
                return result;
            }
        }

        return expandResult(result);
    }

    private void initLocalContext(final String value) throws JsonLdError {
        /*
         * 3. If local context is not null, it contains an entry with a key that equals
         * value, and the value of the entry for value in defined is not true, invoke
         * the Create Term Definition algorithm, passing active context, local context,
         * value as term, and defined. This will ensure that a term definition is
         * created for value in active context during Context Processing
         */
        if (localContext != null && localContext.containsKey(value)) {

            JsonValue entryValue = localContext.get(value);

            if (JsonUtils.isString(entryValue)) {

                String entryValueString = ((JsonString) entryValue).getString();

                if (!defined.containsKey(entryValueString) || Boolean.FALSE.equals(defined.get(entryValueString))) {
                    activeContext.newTerm(localContext, defined).create(value);
                }
            }
        }
    }

    private String initPropertyContext(final String prefix, final String suffix, final String result) throws JsonLdError {

        // 6.3.
        if (localContext != null && !Boolean.TRUE.equals(defined.get(prefix)) && localContext.containsKey(prefix)) {
            activeContext.newTerm(localContext, defined).create(prefix);
        }

        // 6.4.
        final TermDefinition prefixDefinition = activeContext.getPrefix(prefix);

        if (prefixDefinition != null && prefixDefinition.isPrefix()) {
            String uriMapping = prefixDefinition.getUriMapping();
            if (uriMapping != null) {
                return uriMapping + suffix;
            }
        }

        return result;
    }

    private String expandResult(final String result) {
        // 7. If vocab is true, and active context has a vocabulary mapping,
        // return the result of concatenating the vocabulary mapping with value.
        if (vocab && activeContext.getVocabularyMapping() != null) {

            return activeContext.getVocabularyMapping().concat(result);

            // 8.
        } else if (documentRelative) {

            return UriResolver.resolve(activeContext.getBaseUri(), result);
        }

        // 9.
        return result;
    }

    public UriExpansion uriValidation(boolean uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }
}
