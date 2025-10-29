package com.apicatalog.jsonld.loader;

import java.io.Reader;
import java.net.URI;
import java.util.Optional;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.PolyNode;

public class QuadSetDocument implements Document {

    URI documentUrl;
    OrderedQuadSet content;

    public static QuadSetDocument readNQuads(Reader reader) throws NQuadsReaderException, RdfConsumerException {

        QuadSetDocument document = new QuadSetDocument();

        document.content = new OrderedQuadSet();

        new NQuadsReader(reader).provide(new QuadAcceptor(document.content));

        return document;
    }

    @Override
    public MediaType contentType() {
        return MediaType.N_QUADS;
    }

    @Override
    public URI contextUrl() {
        return null;
    }

    @Override
    public void setContextUrl(URI contextUrl) {
    }

    @Override
    public URI documentUrl() {
        return documentUrl;
    }

    @Override
    public void setDocumentUrl(URI documentUrl) {
        this.documentUrl = documentUrl;
    }

    @Override
    public Optional<String> profile() {
        return Optional.empty();
    }

    //FIXME
    public RdfQuadSet contentX() {
        return content;
    }
    
    @Override
    public PolyNode content() {
        // TODO Auto-generated method stub
        return null;
    }

    public void setContent(OrderedQuadSet content) {
        this.content = content;
    }
}
