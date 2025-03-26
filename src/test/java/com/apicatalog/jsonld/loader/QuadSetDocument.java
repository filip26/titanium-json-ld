package com.apicatalog.jsonld.loader;

import java.io.Reader;
import java.net.URI;
import java.util.Optional;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.QuadSet;

public class QuadSetDocument implements Document {

    URI documentUrl;
    QuadSet content;

    public static QuadSetDocument readNQuads(Reader reader) throws NQuadsReaderException, RdfConsumerException {

        QuadSetDocument document = new QuadSetDocument();

        document.content = new QuadSet();

        new NQuadsReader(reader).provide(new QuadAcceptor(document.content));

        return document;
    }

    @Override
    public MediaType getContentType() {
        return MediaType.N_QUADS;
    }

    @Override
    public URI getContextUrl() {
        return null;
    }

    @Override
    public void setContextUrl(URI contextUrl) {
    }

    @Override
    public URI getDocumentUrl() {
        return documentUrl;
    }

    @Override
    public void setDocumentUrl(URI documentUrl) {
        this.documentUrl = documentUrl;
    }

    @Override
    public Optional<String> getProfile() {
        return Optional.empty();
    }

    public QuadSet getContent() {
        return content;
    }

    public void setContent(QuadSet content) {
        this.content = content;
    }
}
