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
package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.concurrent.CompletableFuture;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;

public final class UriBaseRewriter implements DocumentLoader {

    private final String sourceBase;
    private final String targetBase;

    private final DocumentLoader loader;

    public UriBaseRewriter(final String sourceBase, final String targetBase, final DocumentLoader loader) {
        this.sourceBase = sourceBase;
        this.targetBase = targetBase;

        this.loader = loader;
    }

    @Override
    public CompletableFuture<Document> loadDocument(final URI url, final DocumentLoaderOptions options) throws JsonLdError {

        final String sourceUrl = url.toString();

        if (!sourceUrl.startsWith(sourceBase)) {
            return loader.loadDocument(url, options);
        }

        final String relativePath = sourceUrl.substring(sourceBase.length());

        return loader.loadDocument(URI.create(targetBase + relativePath), options)
                .thenCompose(remoteDocument -> {

                    if (remoteDocument == null) {
                        return CompletableFuture.failedFuture(new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED));
                    }

                    if (remoteDocument.getDocumentUrl() != null && remoteDocument.getDocumentUrl().toString().startsWith(targetBase)) {

                        final String remoteRelativePath = remoteDocument.getDocumentUrl().toString().substring(targetBase.length());
                        remoteDocument.setDocumentUrl(URI.create(sourceBase + remoteRelativePath));

                    }
                    return CompletableFuture.completedFuture(remoteDocument);

                });

    }
}