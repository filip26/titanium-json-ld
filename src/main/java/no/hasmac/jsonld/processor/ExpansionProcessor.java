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
package no.hasmac.jsonld.processor;

import java.net.URI;
import java.util.Optional;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.expansion.Expansion;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-expand">JsonLdProcessor.expand()</a>
 *
 */
public final class ExpansionProcessor {

    ExpansionProcessor() {
    }

    public static JsonArray expand(final URI input, final JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return expand(remoteDocument, options, false);
    }

    public static JsonArray expand(Document input, final JsonLdOptions options, boolean frameExpansion) throws JsonLdError {

        if (input == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "RemoteDocument is null.");
        }

        final JsonStructure jsonStructure = input
                                                .getJsonContent()
                                                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document is not pased JSON."));

        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.

        URI baseUri = null;
        URI baseUrl = null;

        if (input.getDocumentUrl() != null) {
            baseUrl = input.getDocumentUrl();
            baseUri = baseUrl;
        }

        if (baseUrl == null) {
            baseUrl = options.getBase();
        }
        if (options.getBase() != null) {
            baseUri = options.getBase();
        }

        ActiveContext activeContext = new ActiveContext(baseUri, baseUrl, options);

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.getExpandContext() != null) {

            final Optional<JsonStructure> contextValue = options.getExpandContext().getJsonContent();

            if (contextValue.isPresent()) {
                activeContext = updateContext(activeContext, contextValue.get(), baseUrl);
            }
        }

        // 7.
        if (input.getContextUrl() != null) {
            activeContext = activeContext
                                .newContext()
                                .create(JsonProvider.instance().createValue(input.getContextUrl().toString()), input.getContextUrl());
        }

        // 8.
        JsonValue expanded = Expansion
                                .with(activeContext, jsonStructure, null, baseUrl)
                                .frameExpansion(frameExpansion)
                                .ordered(options.isOrdered())
                                .compute();

        // 8.1
        if (JsonUtils.isObject(expanded)) {

            final JsonObject object = expanded.asJsonObject();

            if (object.size() == 1 && object.containsKey(Keywords.GRAPH)) {
                expanded = object.get(Keywords.GRAPH);
            }
        }

        // 8.2
        if (JsonUtils.isNull(expanded)) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }

        // 8.3
        return JsonUtils.toJsonArray(expanded);
    }

    private static ActiveContext updateContext(final ActiveContext activeContext, final JsonValue expandedContext, final URI baseUrl) throws JsonLdError {

      if (JsonUtils.isArray(expandedContext)) {

          if (expandedContext.asJsonArray().size() == 1) {

              final JsonValue value = expandedContext.asJsonArray().iterator().next();

              if (JsonUtils.containsKey(value, Keywords.CONTEXT)) {

                  return activeContext
                          .newContext()
                              .create(
                                  value.asJsonObject().get(Keywords.CONTEXT),
                                  baseUrl);
              }
          }

          return activeContext.newContext().create(expandedContext, baseUrl);

      } else if (JsonUtils.containsKey(expandedContext, Keywords.CONTEXT)) {

          return activeContext
                  .newContext()
                      .create(
                          expandedContext.asJsonObject().get(Keywords.CONTEXT),
                          baseUrl);

      }
      return activeContext.newContext().create(JsonProvider.instance().createArrayBuilder().add(expandedContext).build(), baseUrl);
    }

}
