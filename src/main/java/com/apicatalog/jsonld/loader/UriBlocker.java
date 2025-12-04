package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.Set;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.LoaderException.ErrorCode;

public final class UriBlocker implements DocumentLoader {

    private final Set<URI> uris;

    private final Set<String> bases;

    private final DocumentLoader loader;

    private final boolean blacklist;

    private UriBlocker(
            boolean blacklist,
            Set<URI> uris,
            Set<String> bases,
            DocumentLoader loader) {
        this.blacklist = blacklist;
        this.uris = uris;
        this.bases = bases;
        this.loader = loader;

    }

    public static Builder newWhitelist(DocumentLoader loader) {
        // TODO
        return null;
    }

    public static Builder newBlaclist(DocumentLoader loader) {
        // TODO
        return null;
    }

    @Override
    public Document loadDocument(URI uri, Options options) throws LoaderException {

        var match = uris.contains(uri);

        if (!match && !bases.isEmpty()) {

            final var uriString = uri.toString();

            for (var base : bases) {

                match = uriString.startsWith(base);
                if (match) {
                    break;
                }
            }
        }

        if (blacklist == match) {
            // TODO add specialized code??
            throw new LoaderException(
                    ErrorCode.BLOCKED_URI,
                    uri,
                    blacklist
                            ? "The URI [" + uri + "] is blaclisted"
                            : "The URI [" + uri + "] is not whitelisted, allowed");
        }

        return loader.loadDocument(uri, options);
    }

    public static class Builder {

    }

}
