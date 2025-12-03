/*
 * Copyright 2025 the original author or authors.
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
package com.apicatalog.jsonld.lang;

/**
 * 
 * @since 2.0.0
 */
public final class Terms {

    public static final String XSD_STRING = "http://www.w3.org/2001/XMLSchema#string";

    public static final String XSD_BOOLEAN = "http://www.w3.org/2001/XMLSchema#boolean";

    public static final String XSD_INT = "http://www.w3.org/2001/XMLSchema#int";

    public static final String XSD_INTEGER = "http://www.w3.org/2001/XMLSchema#integer";

    public static final String XSD_LONG = "http://www.w3.org/2001/XMLSchema#long";

    public static final String XSD_FLOAT = "http://www.w3.org/2001/XMLSchema#float";

    public static final String XSD_DOUBLE = "http://www.w3.org/2001/XMLSchema#double";

    public static final String XSD_DECIMAL = "http://www.w3.org/2001/XMLSchema#decimal";

    public static final String RDF_DIRECTION = "http://www.w3.org/1999/02/22-rdf-syntax-ns#direction";

    public static final String RDF_TYPE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

    public static final String RDF_NIL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

    public static final String RDF_FIRST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";

    public static final String RDF_REST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";

    public static final String RDF_JSON = "http://www.w3.org/1999/02/22-rdf-syntax-ns#JSON";

    public static final String RDF_LANG_STRING = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";

    public static final String RDF_DIR_LANG_STRING = "http://www.w3.org/1999/02/22-rdf-syntax-ns#dirLangString";

    public static final String RDF_LIST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#List";

    public static final String RDF_VALUE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#value";

    public static final String RDF_LANGUAGE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#language";

    public static final String RDF_I18N_BASE = "https://www.w3.org/ns/i18n#";

    /*
     * @see <a href="https://www.w3.org/TR/json-ld11/#iana-considerations">IANA
     *      Considerations</a>
     */

    public static final String PROFILE_EXPANDED = "http://www.w3.org/ns/json-ld#expanded";

    public static final String PROFILE_COMPACTED = "http://www.w3.org/ns/json-ld#compacted";

    public static final String PROFILE_CONTEXT = "http://www.w3.org/ns/json-ld#context";

    public static final String PROFILE_FLATTENED = "http://www.w3.org/ns/json-ld#flattened";

    public static final String PROFILE_FRAME = "http://www.w3.org/ns/json-ld#frame";

    public static final String PROFILE_FRAMED = "http://www.w3.org/ns/json-ld#framed";

    private Terms() {
    }
}