package no.hasmac.jsonld.loader;

import no.hasmac.jsonld.http.DefaultHttpClient;
import no.hasmac.jsonld.http.HttpClient;

public class HttpLoader extends DefaultHttpLoader {

    private static final HttpLoader INSTANCE = new HttpLoader(DefaultHttpClient.defaultInstance());

    /**
     * @deprecated use <code>HttpLoader(no.hasmac.jsonld.http.HttpClient httpClient)</code>
     *
     * @param httpClient
     */
    @Deprecated(since = "1.0.3")
    public HttpLoader(java.net.http.HttpClient httpClient) {
        this(httpClient, MAX_REDIRECTIONS);
    }

    /**
     * @deprecated use <code>HttpLoader(no.hasmac.jsonld.http.HttpClient httpClient, int maxRedirection)</code>
     *
     * @param httpClient
     * @param maxRedirections
     */
    @Deprecated(since = "1.0.3")
    public HttpLoader(java.net.http.HttpClient httpClient, int maxRedirections) {
        this(new DefaultHttpClient(httpClient), maxRedirections);
    }

    public HttpLoader(HttpClient httpClient) {
        super(httpClient);
    }

    public HttpLoader(HttpClient httpClient, int maxRedirections) {
        super(httpClient, maxRedirections);
    }

    public static DocumentLoader defaultInstance() {
        return INSTANCE;
    }
}
