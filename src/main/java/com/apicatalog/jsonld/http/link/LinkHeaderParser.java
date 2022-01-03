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
package com.apicatalog.jsonld.http.link;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.StringUtils;
import com.apicatalog.jsonld.http.HttpAlphabet;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.uri.UriResolver;

/**
 *
 * @see <a href="https://tools.ietf.org/html/rfc8288#appendix-B">Appendix B.  Algorithms for Parsing Link Header Fields</a>
 *
 */
final class LinkHeaderParser {

    private static final String REL = "rel";
    private static final String ANCHOR = "anchor";
    private static final String TYPE = "type";

    private enum State { INIT, URI_REF, PARAMS, PARAM_NAME_BEGIN, PARAM_NAME, PARAM_NAME_END, PARAM_VALUE,
            STRING_VALUE, LITERAL_VALUE, ESCAPE, UNEXPECTED }

    private URI baseUri;

    private final StringBuilder valueBuilder;

    private List<Link> links;
    private State state;

    private boolean foundLink;
    private URI targetUri;
    private String attributeName;
    private String attributeValue;
    private Map<String, List<LinkAttribute>> attributes;

    public LinkHeaderParser(final URI baseUri) {
        this.baseUri = baseUri;
        this.valueBuilder = new StringBuilder();
    }

    public List<Link> parse(String httpLink) {

        resetState(baseUri);

        final char[] linkHeader = httpLink.toCharArray();

        for (final char ch : linkHeader) {

            switch (state) {
                case INIT:
                    initParser(ch);
                    break;

                case URI_REF:
                    parseTargetUri(ch);
                    break;

                case PARAMS:
                    parseParameters(ch);
                    break;

                case PARAM_NAME_BEGIN:
                    parseParamNameBegin(ch);
                    break;

                case PARAM_NAME:
                    parseParamName(ch);
                    break;

                case PARAM_NAME_END:
                    parseParamNameEnd(ch);
                    break;

                case PARAM_VALUE:
                    parseParamValue(ch);
                    break;

                case LITERAL_VALUE:
                    parseLiteral(ch);
                    break;

                case STRING_VALUE:
                    parseString(ch);
                    break;

                case ESCAPE:
                    escape(ch);
                    break;

                default:
                    addParameter();
                    addLink();
                    return links;
            }
        }

        return sweep();
    }

    private final List<Link> sweep() {
        switch (state) {
        case PARAM_NAME_BEGIN:
        case PARAM_NAME:
        case PARAM_NAME_END:
            if (valueBuilder.length() > 0) {
                attributeName = StringUtils.stripTrailing(valueBuilder.toString());
            }
            break;

        case LITERAL_VALUE:
            if (valueBuilder.length() > 0) {
                attributeValue = StringUtils.stripTrailing(valueBuilder.toString());
            }
            break;

        case UNEXPECTED:
            return links;

        default:
            break;
        }

        addParameter();
        addLink();

        return links;
    }

    private final void addLink() {
        if (foundLink) {

            Set<String> rel = Collections.emptySet();
            URI context = null;
            MediaType type = null;

            if (attributes.containsKey(REL) && attributes.get(REL) != null) {
                rel = new HashSet<>(Arrays.asList(StringUtils.strip(attributes.get(REL).get(0).value()).split("[\\s\\t]+")));
                attributes.remove(REL);
            }
            if (attributes.containsKey(ANCHOR) && attributes.get(ANCHOR) != null) {
                context = UriResolver.resolveAsUri(baseUri, StringUtils.strip(attributes.get(ANCHOR).get(0).value()));
                attributes.remove(ANCHOR);
            }
            if (attributes.containsKey(TYPE) && attributes.get(TYPE) != null) {
                type = MediaType.of(attributes.get(TYPE).get(0).value());
                if (type != null) {
                    attributes.remove(TYPE);
                }
            }

            links.add(new Link(context, targetUri, rel, type, new LinkAttributes(attributes)));
            targetUri = null;
            attributes = new LinkedHashMap<>();
            foundLink = false;
        }
    }

    private final void addParameter() {
        if (attributeName != null) {
            if (attributeValue != null) {
                attributes
                        .computeIfAbsent(attributeName, l -> new ArrayList<>())
                        .add(new LinkAttribute(attributeName, attributeValue));

                attributeValue = null;

            } else {

                attributes
                        .computeIfAbsent(attributeName, l -> new ArrayList<>())
                        .add(new LinkAttribute(attributeName));
            }

            attributeName = null;
        }
    }

    private final void resetState(URI baseUri) {
        this.baseUri = baseUri;
        this.links = new ArrayList<>();
        this.attributes = new LinkedHashMap<>();
        this.state = State.INIT;

        this.foundLink = false;
        this.targetUri = null;
        this.attributeName = null;
        this.attributeValue = null;
    }

    private final void initParser(final char ch) {
        // skip white-spaces
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (ch == '<') {
            valueBuilder.setLength(0);
            state = State.URI_REF;
            return;
        }
        state = State.UNEXPECTED;
    }

    private final void parseTargetUri(final char ch) {
        if (ch != '>') {
            // skip leading white-spaces
            if (valueBuilder.length() > 0 || HttpAlphabet.WHITESPACE.negate().test(ch)) {
                valueBuilder.append(ch);
            }
            return;
        }
        targetUri = UriResolver.resolveAsUri(baseUri, StringUtils.stripTrailing(valueBuilder.toString()));
        foundLink = true;
        state = State.PARAMS;
    }

    private final void parseParameters(final char ch) {
        // skip leading white-spaces
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        // next link
        if (ch == ',') {
            addLink();
            state = State.INIT;
            return;
        }
        // next attribute
        if (ch == ';') {
            valueBuilder.setLength(0);
            state = State.PARAM_NAME_BEGIN;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void parseParamNameBegin(final char ch) {
        // skip leading white-spaces
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (HttpAlphabet.T_CHAR.test(ch)) {
            valueBuilder.append(ch);
            state = State.PARAM_NAME;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void parseParamName(final char ch) {
        if (ch == '=') {
            attributeName = valueBuilder.toString();
            valueBuilder.setLength(0);
            state = State.PARAM_VALUE;
            return;
        }
        if (ch == ';') {
            attributeName = valueBuilder.toString();
            valueBuilder.setLength(0);
            addParameter();
            return;
        }
        if (ch == ',') {
            attributeName = valueBuilder.toString();
            addParameter();
            addLink();
            state = State.INIT;
            return;
        }
        if (HttpAlphabet.T_CHAR.test(ch)) {
            valueBuilder.append(ch);
            return;
        }
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            attributeName = valueBuilder.toString();
            valueBuilder.setLength(0);
            state = State.PARAM_NAME_END;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void parseParamNameEnd(final char ch) {
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (ch == '=') {
            state = State.PARAM_VALUE;
            return;
        }
        if (ch == ';') {
            addParameter();
            state = State.PARAM_NAME_BEGIN;
            return;
        }
        if (ch == ',') {
            addParameter();
            addLink();
            state = State.INIT;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void parseParamValue(final char ch) {
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            return;
        }
        if (ch == '"') {
            state = State.STRING_VALUE;
            return;
        }
        if (HttpAlphabet.T_CHAR.test(ch)) {
            valueBuilder.append(ch);
            state = State.LITERAL_VALUE;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }


    private final void parseString(final char ch) {
        if (ch == '"') {
            attributeValue = valueBuilder.toString();
            addParameter();
            state = State.PARAMS;
            return;
        }
        if (ch == '\\') {
            state = State.ESCAPE;
            return;
        }
        if (HttpAlphabet.QD_TEXT.test(ch)) {
            valueBuilder.append(ch);
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void parseLiteral(final char ch) {

        if (ch == ';') {
            attributeValue = valueBuilder.toString();
            valueBuilder.setLength(0);
            addParameter();
            state = State.PARAM_NAME;
            return;

        } else if (ch == ',') {
            attributeValue = valueBuilder.toString();
            addParameter();
            addLink();
            state = State.INIT;
            return;
        }
        if (HttpAlphabet.T_CHAR.test(ch)) {
            valueBuilder.append(ch);
            return;
        }
        if (HttpAlphabet.WHITESPACE.test(ch)) {
            attributeValue = valueBuilder.toString();
            addParameter();
            state = State.PARAMS;
            return;
        }
        addLink();
        state = State.UNEXPECTED;
    }

    private final void escape(final char ch) {
        if (ch == 't') {
            valueBuilder.append('\t');

        } else {
            valueBuilder.append(ch);
        }

        state = State.STRING_VALUE;
    }
}