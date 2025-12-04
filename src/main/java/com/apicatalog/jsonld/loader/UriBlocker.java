package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.HashSet;
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
        return new Builder(loader, false);
    }

    public static Builder newBlacklist(DocumentLoader loader) {
        return new Builder(loader, true);
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
            throw new LoaderException(
                    ErrorCode.BLOCKED_URI,
                    uri,
                    blacklist
                            ? "The URI [" + uri + "] is black-listed"
                            : "The URI [" + uri + "] is not white-listed");
        }

        return loader.loadDocument(uri, options);
    }

    public static class Builder {

        private final DocumentLoader loader;
        private final boolean blacklist;
        private final Set<URI> uris;
        private final Set<String> bases;

        private Builder(DocumentLoader loader, boolean blacklist) {
            this.loader = loader;
            this.blacklist = blacklist;
            this.uris = new HashSet<URI>();
            this.bases = new HashSet<String>();
        }

        public Builder uri(URI uri) {
            uris.add(uri);
            return this;
        }

        public Builder base(String base) {
            bases.add(base);
            return this;
        }

        public UriBlocker build() {
            return new UriBlocker(
                    blacklist,
                    Set.copyOf(uris),
                    Set.copyOf(bases),
                    loader);
        }

    }

}
