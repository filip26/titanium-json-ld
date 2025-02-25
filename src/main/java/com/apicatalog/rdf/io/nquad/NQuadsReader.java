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
package com.apicatalog.rdf.io.nquad;

import java.io.Reader;
import java.util.Arrays;

import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfDatasetSupplier;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.error.RdfReaderException;
import com.apicatalog.rdf.io.nquad.Tokenizer.Token;
import com.apicatalog.rdf.io.nquad.Tokenizer.TokenType;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;

/**
 *
 * @see <a href="https://www.w3.org/TR/n-quads/">RDF 1.1. N-Quads</a>
 *
 */
public class NQuadsReader implements RdfReader {

    protected final Tokenizer tokenizer;

    // runtime
    private String object;
    private String datatype;
    private String langTag;
    private String direction;

    public NQuadsReader(final Reader reader) {
        this.tokenizer = new Tokenizer(reader);
    }

    @Override
    @Deprecated
    public RdfDataset readDataset() throws RdfReaderException {

        RdfDataset dataset = Rdf.createDataset();

        RdfQuadConsumer consumer = new RdfDatasetSupplier(dataset);

        read(consumer);
        
        return dataset;
    }
    
    public void read(RdfQuadConsumer consumer) throws RdfReaderException {
        while (tokenizer.hasNext()) {

            // skip EOL and whitespace
            if (tokenizer.accept(Tokenizer.TokenType.END_OF_LINE)
                    || tokenizer.accept(Tokenizer.TokenType.WHITE_SPACE)
                    || tokenizer.accept(Tokenizer.TokenType.COMMENT)) {

                continue;
            }

            reaStatement(consumer);
        }        
    }

    private void reaStatement(RdfQuadConsumer consumer) throws RdfReaderException {

        String subject = readResource("Subject");

        skipWhitespace(0);

        String predicate = readResource("Predicate");

        skipWhitespace(0);

        readObject();

        String graphName = null;

        skipWhitespace(0);

        if (TokenType.IRI_REF == tokenizer.token().getType()) {

            final String graphNameIri = tokenizer.token().getValue();

            assertAbsoluteIri(graphNameIri, "Graph name");

            graphName = graphNameIri;
            tokenizer.next();
            skipWhitespace(0);
        }

        if (TokenType.BLANK_NODE_LABEL == tokenizer.token().getType()) {

            graphName = "_:".concat(tokenizer.token().getValue());
            tokenizer.next();
            skipWhitespace(0);
        }

        if (TokenType.END_OF_STATEMENT != tokenizer.token().getType()) {
            unexpected(tokenizer.token(), TokenType.END_OF_STATEMENT);
        }

        tokenizer.next();

        skipWhitespace(0);

        // skip comment
        if (TokenType.COMMENT == tokenizer.token().getType()) {
            tokenizer.next();

            // skip end of line
        } else if (TokenType.END_OF_LINE != tokenizer.token().getType() && TokenType.END_OF_INPUT != tokenizer.token().getType()) {
            unexpected(tokenizer.token(), TokenType.END_OF_LINE, TokenType.END_OF_INPUT);
            tokenizer.next();
        }

        if (direction != null || langTag != null) {
            consumer.quad(subject, predicate, object, langTag, direction, graphName);

        } else if (datatype != null) {
            consumer.quad(subject, predicate, object, datatype, graphName);

        } else {
            consumer.quad(subject, predicate, object, graphName);
        }
    }

    private String readResource(String name) throws RdfReaderException {

        final Token token = tokenizer.token();

        if (TokenType.IRI_REF == token.getType()) {

            tokenizer.next();

            final String iri = token.getValue();

            assertAbsoluteIri(iri, name);

            return iri;
        }

        if (TokenType.BLANK_NODE_LABEL == token.getType()) {

            tokenizer.next();

            return "_:".concat(token.getValue());
        }

        return unexpected(token);
    }

    private void readObject() throws RdfReaderException {

        object = null;
        datatype = null;
        langTag = null;
        direction = null;

        Token token = tokenizer.token();

        if (TokenType.IRI_REF == token.getType()) {
            tokenizer.next();

            final String iri = token.getValue();

            assertAbsoluteIri(iri, "Object");

            object = iri;
            return;
        }

        if (TokenType.BLANK_NODE_LABEL == token.getType()) {

            tokenizer.next();

            object = "_:".concat(token.getValue());
            return;
        }

        readLiteral();
    }

    private void readLiteral() throws RdfReaderException {

        Token value = tokenizer.token();

        if (TokenType.STRING_LITERAL_QUOTE != value.getType()) {
            unexpected(value);
        }

        tokenizer.next();

        skipWhitespace(0);

        if (TokenType.LANGTAG == tokenizer.token().getType()) {

            String langTag = tokenizer.token().getValue();

            tokenizer.next();

            this.object = value.getValue();
            this.langTag = langTag;

            return;

        } else if (TokenType.LITERAL_DATA_TYPE == tokenizer.token().getType()) {

            tokenizer.next();
            skipWhitespace(0);

            Token attr = tokenizer.token();

            if (TokenType.IRI_REF == attr.getType()) {

                tokenizer.next();

                final String iri = attr.getValue();

                assertAbsoluteIri(iri, "DataType");

                if (RdfConstants.I18N_BASE.equals(iri)) {
                    this.object = value.getValue();

                    String[] langDir = iri.substring(RdfConstants.I18N_BASE.length()).split("_");

                    // TODO assert

                    this.datatype = iri;
                    this.langTag = langDir[0];
                    this.direction = langDir[1];

                    return;
                }

                this.object = value.getValue();
                this.datatype = iri;
//                Rdf.createTypedString(value.getValue(), iri);
                return;
            }

            unexpected(attr);
        }

        this.object = value.getValue();
        this.datatype = XsdConstants.STRING;
    }

    private static final <T> T unexpected(Token token, TokenType... types) throws RdfReaderException {
        throw new RdfReaderException(
                "Unexpected token " + token.getType() + (token.getValue() != null ? "[" + token.getValue() + "]" : "") + ". "
                        + "Expected one of " + Arrays.toString(types) + ".");
    }

    private void skipWhitespace(int min) throws RdfReaderException {

        int count = 0;

        while (tokenizer.accept(TokenType.WHITE_SPACE)) {
            count++;
        }

        if (count < min) {
            unexpected(tokenizer.token());
        }
    }

    private static final void assertAbsoluteIri(final String iri, final String what) throws RdfReaderException {
        if (UriUtils.isNotAbsoluteUri(iri, UriValidationPolicy.Full)) {
            throw new RdfReaderException(what + " must be an absolute IRI [" + iri + "]. ");
        }
    }
}
