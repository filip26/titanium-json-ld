package no.hasmac.jsonld.json;

/**
 * Instead of using the Jakarta Json static class to create builders, it
 * is much faster to keep track of the provider in a static instance.  This
 * avoids the overhead of doing a service lookup on each call.  This improves
 * performance dramatically (about 200 to 300% for most calls).
 */
public class JsonProvider {

	private static jakarta.json.spi.JsonProvider provider;

	private JsonProvider() {}

	public static jakarta.json.spi.JsonProvider instance() {
		if (provider == null) {
			provider = jakarta.json.spi.JsonProvider.provider();
		}
		return provider;
	}
}
