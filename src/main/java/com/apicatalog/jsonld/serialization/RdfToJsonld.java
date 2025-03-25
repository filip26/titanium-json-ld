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
package com.apicatalog.jsonld.serialization;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.api.RdfConsumerException;

import jakarta.json.JsonArray;

@Deprecated
public final class RdfToJsonld {

    private final RdfDataset dataset;

    // optional
    private boolean ordered;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    private boolean useRdfType;
    private UriValidationPolicy uriValidation;

    private JsonLdVersion processingMode;

    private RdfToJsonld(RdfDataset dataset) {
        this.dataset = dataset;

        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;
        this.uriValidation = JsonLdOptions.DEFAULT_URI_VALIDATION;
    }

    public static final RdfToJsonld with(final RdfDataset dataset) {
        return new RdfToJsonld(dataset);
    }

    public RdfToJsonld ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public RdfToJsonld rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public RdfToJsonld useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }

    public RdfToJsonld useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }

    public RdfToJsonld processingMode(JsonLdVersion processingMode) {
        this.processingMode = processingMode;
        return this;
    }

    /**
     * @deprecated since 1.5.0, use
     *             <code>RdfToJsonld#uriValidation(com.apicatalog.jsonld.uri.UriValidationPolicy)</code>
     */
    @Deprecated
    public RdfToJsonld uriValidation(boolean enabled) {
        return uriValidation(enabled ? UriValidationPolicy.Full : UriValidationPolicy.SchemeOnly);
    }

    public RdfToJsonld uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    public JsonArray build() throws JsonLdError {

        final QuadsToJsonld toLd = new QuadsToJsonld();
        toLd.ordered(ordered);
        toLd.processingMode(processingMode);
        toLd.rdfDirection(rdfDirection);
        toLd.uriValidation(uriValidation);
        toLd.useNativeTypes(useNativeTypes);
        toLd.useRdfType(useRdfType);

        try {

            for (final RdfNQuad quad : dataset.toList()) {
                if (quad.getObject().isLiteral()) {
                    toLd.quad(
                            quad.getSubject().getValue(),
                            quad.getPredicate().getValue(),
                            quad.getObject().getValue(),
                            quad.getObject().asLiteral().getDatatype(),
                            quad.getObject().asLiteral().getLanguage().orElse(null),
                            null,
                            quad.getGraphName()
                                    .map(RdfResource::getValue)
                                    .orElse(null));

                    continue;
                }
                toLd.quad(
                        quad.getSubject().getValue(),
                        quad.getPredicate().getValue(),
                        quad.getObject().getValue(),
                        null, null, null,
                        quad.getGraphName()
                                .map(RdfResource::getValue)
                                .orElse(null));
            }
        } catch (RdfConsumerException e) {
            if (e.getCause() instanceof JsonLdError) {
                throw (JsonLdError)e.getCause();
            }
            throw new JsonLdError(JsonLdErrorCode.UNSPECIFIED, e);
        }

        return toLd.build();
    }
}