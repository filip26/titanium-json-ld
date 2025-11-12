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

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.expansion.Expansion;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-expand">JsonLdProcessor.expand()</a>
 *
 */
public final class Expander {

    public static final Collection<?> expand(
            final Document document,
            final Options options,
            final Execution runtime) throws JsonLdException {

        return Expander.expand(
                document.content(),
                Expander.context(
                        document.url(),
                        document.context(),
                        options,
                        runtime),
                Expander.baseUrl(document.url(), options),
                options,
                runtime);
    }

    public static final Collection<?> expand(
            final TreeIO document,
            final Options options,
            final Execution runtime) throws JsonLdException {

        return Expander.expand(
                document,
                Expander.context(null, null, options, runtime),
                Expander.baseUrl(null, options),
                options,
                runtime);
    }

    
    public static final Collection<?> expand(
            final TreeIO node,
            final Context context,
            final URI baseUrl,
            final Options options,
            final Execution runtime) throws JsonLdException {
        return expand(node, context, baseUrl, false, options, runtime);
    }

    public static final Collection<?> expandFrame(
            final Document document,
            final Options options,
            final Execution runtime) throws JsonLdException {

        return Expander.expandFrame(
                document.content(),
                Expander.context(
                        document.url(),
                        document.context(),
                        options,
                        runtime),
                Expander.baseUrl(document.url(), options),
                options,
                runtime);
    }

    public static final Collection<?> expandFrame(
            final TreeIO node,
            final Context context,
            final URI baseUrl,
            final Options options,
            final Execution runtime) throws JsonLdException {
        return expand(node, context, baseUrl, true, options, runtime);
    }

    public static final Context context(
            final URI document,
            final URI context,
            final Options options,
            final Execution runtime) throws JsonLdException {

        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.
        URI baseUrl = baseUrl(document, options);
        URI baseUri = document;

        if (options.base() != null) {
            baseUri = options.base();
        }

        var builder = new Context.Builder(
                baseUri,
                baseUrl,
                options.mode())
                .runtime(runtime)
                .loader(options.loader());

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.expandContext() != null) {
            final var expandContext = options.expandContext().content();

            builder.update(
                    extractExpansionContext(expandContext),
                    expandContext.adapter(),
                    true,
                    baseUrl);
        }

        // 7.
        if (context != null) {
            builder.update(
                    context.toString(),
                    NativeAdapter.instance(),
                    true,
                    context);
        }

        return builder.build();
    }

    public static final URI baseUrl(URI document, Options options) {
        return document != null
                ? document
                : options.base();
    }

    private static final Collection<?> expand(
            final TreeIO node,
            final Context context,
            final URI baseUrl,
            boolean frameExpansion,
            final Options options,
            final Execution runtime) throws JsonLdException {

        // 8.
        var expanded = Expansion.expand(
                context,
                node.node(),
                node.adapter(),
                null,
                new Params(frameExpansion, false, baseUrl, options, runtime));

        // 8.1
        if (expanded instanceof Map map
                && map.size() == 1
                && map.containsKey(Keywords.GRAPH)) {
            expanded = map.get(Keywords.GRAPH);
        }

        // 8.2
        if (expanded == null) {
            return Set.of();
        }

        if (expanded instanceof Collection<?> collection) {
            return List.copyOf(collection);
        }

        // 8.3
        return Set.of(expanded);
    }

    //TODO replace with Context.unwrap?
    @Deprecated
    private static Object extractExpansionContext(final TreeIO context) throws JsonLdException {
        // Defensive null check — optional, but safer in utility code
        Objects.requireNonNull(context, "context must not be null");

        // Case 1: Collection
        if (context.isCollection()) {
            // Single-element collection: check if it’s a map with @context key
            if (context.isSingleElement()) {
                final var element = context.singleElement();
                final var adapter = context.adapter();

                if (adapter.isMap(element) && adapter.keys(element).contains(Keywords.CONTEXT)) {
                    return adapter.property(Keywords.CONTEXT, element);
                }
            }
            // Otherwise return the raw node
            return context.node();
        }

        // Case 2: Map with @context key
        if (context.isMap() && context.keys().contains(Keywords.CONTEXT)) {
            return context.property(Keywords.CONTEXT);
        }

        return context.node();
//        // Case 3: Native adapter — wrap as list
//        if (NativeAdapter.instance().isCompatibleWith(context.adapter())) {
//            return List.of(context.node());
//        }
//
//        // Case 4: Fallback — materialize node and wrap
//        return List.of(NativeMaterializer.node(context));
    }
}
