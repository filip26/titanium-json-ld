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
package no.hasmac.rdf.impl;

import java.util.Objects;
import java.util.Optional;

import no.hasmac.rdf.RdfLiteral;
import no.hasmac.rdf.lang.RdfConstants;
import no.hasmac.rdf.lang.XsdConstants;

final class RdfLiteralImpl implements RdfLiteral {

    private final String value;

    private final String langTag;

    private final String dataType;

    protected RdfLiteralImpl(String value) {
        this(value, null, null);
    }

    protected RdfLiteralImpl(String value, String langTag, String datatype) {
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
        if (getClass() != obj.getClass()) {
            return false;
        }
        RdfLiteralImpl other = (RdfLiteralImpl) obj;
        return Objects.equals(dataType, other.dataType) && Objects.equals(langTag, other.langTag)
                && Objects.equals(value, other.value);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();

        builder.append(value);

        if (langTag != null) {
            builder.append('@');
            builder.append(langTag);

        } else if (dataType != null) {
            builder.append("^^");
            builder.append(dataType);
        }

        return builder.toString();
    }

    private static String datatype(String langTag, String datatype) {
        if (datatype != null) {
            return datatype;
        }
        return langTag == null ? XsdConstants.STRING : RdfConstants.LANG_STRING;
    }
}
