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
package com.apicatalog.rdf.impl;

import java.util.Objects;
import java.util.Optional;

import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;
import com.apicatalog.rdf.nquads.NQuadsAlphabet;

/**
 * This class is deprecated as of version 1.7.0.
 * <p>
 * Please use
 * <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium RDF
 * Primitives</a> or any other third-party library to materialize RDF
 * primitives.
 * </p>
 *
 * @see <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium
 *      RDF Primitives</a>
 * @deprecated since 1.7.0 - use an alternative RDF primitives library.
 */
@Deprecated
final class ImmutableRdfLiteral implements RdfLiteral {

    private final String value;

    private final String langTag;

    private final String dataType;

    protected ImmutableRdfLiteral(String value, String langTag, String datatype) {
        this.value = value;
        this.langTag = langTag;
        this.dataType = datatype(langTag, datatype);
    }

    @Override
    public String getValue() {
        return value;
    }

    @Override
    public String getDatatype() {
        return dataType;
    }

    @Override
    public boolean isLiteral() {
        return true;
    }

    @Override
    public Optional<String> getLanguage() {
        return Optional.ofNullable(langTag);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dataType, langTag, value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() == obj.getClass()) {
            ImmutableRdfLiteral other = (ImmutableRdfLiteral) obj;
            return Objects.equals(dataType, other.dataType) && Objects.equals(langTag, other.langTag)
                    && Objects.equals(value, other.value);
        }
        if (!(obj instanceof RdfLiteral)) {
            return false;
        }
        RdfLiteral other = (RdfLiteral) obj;
        return Objects.equals(dataType, other.getDatatype()) && Objects.equals(langTag, other.getLanguage().orElse(null))
                && Objects.equals(value, other.getValue());
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();

        builder.append('"');
        builder.append(NQuadsAlphabet.escape(value));
        builder.append('"');

        if (langTag != null) {
            builder.append('@');
            builder.append(langTag);

        } else if (dataType != null && !XsdConstants.STRING.equals(dataType)) {
            builder.append("^^<");
            builder.append(dataType);
            builder.append('>');
        }

        return builder.toString();
    }

    private static final String datatype(String langTag, String datatype) {
        if (datatype != null) {
            return datatype;
        }
        return langTag == null ? XsdConstants.STRING : RdfConstants.LANG_STRING;
    }
}
