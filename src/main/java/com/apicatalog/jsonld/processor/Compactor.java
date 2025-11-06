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
package com.apicatalog.jsonld.processor;

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.compaction.UriCompaction;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.TreeIO;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class Compactor {

    private Compactor() {
    }

    public static final Collection<?> expand(
            final TreeIO input,
            final Options options,
            final Execution runtime) throws JsonLdException, IOException {

        final var expansionOptions = Options.copyOf(options)
                .ordered(false)
                .extractAllScripts(false);

        return Expander.expand(
                input,
                Expander.context(
                        null,
                        null,
                        options),
                Expander.baseUrl(null, options),
                expansionOptions,
                runtime);
    }

    public static final Map<String, ?> compact(
            final Document input,
            final Document context,
            final Options options,
            final Execution runtime) throws JsonLdException, IOException {

        final var expandedInput = Expander.expand(
                input,
                Options.copyOf(options)
                        .ordered(false)
                        .extractAllScripts(false),
                runtime);

        return compact(
                expandedInput,
                input.url(),
                context.content(),
                options,
                runtime);
    }

    public static final Map<String, ?> compact(
            final Object expanded,
            final URI baseUrl,
            final TreeIO context,
            final Options options,
            final Execution runtime) throws JsonLdException, IOException {

        URI contextBase = baseUrl;

        if (contextBase == null) {
            contextBase = options.base();
        }

        final var localContext = Context.unwrap(context);

        // 6.
//        final JsonValue contextValue = context.getJsonContent()
//                .map(ctx -> JsonUtils.flatten(ctx, Keywords.CONTEXT))
//                .orElse(JsonValue.EMPTY_JSON_OBJECT);

        // 7.
        final var builder = new Context.Builder(options.mode())
                .loader(options.loader())
                .update(localContext, contextBase);

        // 8.
        if (builder.baseUri() == null) {

            if (options.base() != null) {
                builder.baseUri(options.base());

            } else if (options.isCompactToRelative()) {
                builder.baseUri(baseUrl);
            }
        }

        return compact(
                expanded,
                builder.build(),
                localContext,
                options,
                runtime);
    }

    static final Map<String, ?> compact(
            final Object expanded,
            final Context context,
            final TreeIO contextSource,
            final Options options,
            final Execution runtime) throws JsonLdException, IOException {

        var activeContext = context;

//        // 8.
//        if (builder.baseUri() == null) {

//            if (options.getBase() != null) {
//        activeContext = new Context.Builder(context)
//                .baseUri(options.getBase())
//                .build();

//                builder.baseUri(options.getBase());
//
//            } else if (options.isCompactToRelative()) {
//                builder.baseUri(baseUrl);
//            }
//        }
//

        // 9.
        var compactedOutput = Compaction
                .with(activeContext, options, runtime)
                .compactArrays(options.isCompactArrays())
                .ordered(options.isOrdered())
                .compact(expanded);

        // 9.1.
        if (compactedOutput instanceof Collection<?> col) {

            if (col.isEmpty()) {
                return Map.of();

            } else if (!TreeIO.isEmptyOrNull(contextSource)) {
                return Map.of(
                        Keywords.CONTEXT,
                        contextSource,
                        UriCompaction.withVocab(context, Keywords.GRAPH),
                        compactedOutput);

            } else {
                return Map.of(
                        UriCompaction.withVocab(context, Keywords.GRAPH),
                        compactedOutput);
            }
        }

        if (compactedOutput == null || compactedOutput instanceof Map map && map.isEmpty()) {
            return Map.of();
        }

        if (compactedOutput instanceof Map map) {
            @SuppressWarnings("unchecked")
            final var typedMap = (Map<String, ?>) map;

            if (contextSource != null) {
                return Context.inject(typedMap, contextSource);
            }

            return typedMap;
        }

        throw new IllegalStateException();
    }
}
