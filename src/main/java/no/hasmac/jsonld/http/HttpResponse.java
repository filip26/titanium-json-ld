package no.hasmac.jsonld.http;

import java.io.Closeable;
import java.io.InputStream;
import java.util.Collection;
import java.util.Optional;

public interface HttpResponse extends Closeable {

    int statusCode();

    InputStream body();

    Collection<String> links();

    Optional<String> contentType();

    Optional<String> location();

}
