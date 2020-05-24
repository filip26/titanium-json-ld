package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.JavaResourceLoader;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.loader.UrlRewriteLoader;

@RunWith(Parameterized.class)
public class JsonLdExpandTest {

	public static final String RESOURCES_BASE = "/com/github/w3c/json-ld-api/tests/"; 
	
	@Parameterized.Parameter(0)
	public JsonLdTestCase testDefinition;

	@Parameterized.Parameter(1)
	public String testId;
	
	@Parameterized.Parameter(2)
	public String testName;
		
	@Parameterized.Parameter(3)
	public String baseUri;
	
	@Test
	public void testExpand() throws IOException, JsonLdError {

		// skip specVersion == 1.0
		assumeFalse(Version.V1_0.equals(testDefinition.options.specVersion));
		
		// skip normative == false
		assumeTrue(testDefinition.options.normative == null || testDefinition.options.normative);
		
		final LoadDocumentCallback loader = 
				new UrlRewriteLoader(
							baseUri, 
							"classpath:" + RESOURCES_BASE,
							new JavaResourceLoader()
						);
		
		JsonLdOptions options = new JsonLdOptions();
		options.setOrdered(true);
		options.setDocumentLoader(loader);
		
		testDefinition.options.setup(options);

		JsonValue result = null;

		try {
			
			result = JsonLd.createProcessor().expand(URI.create(baseUri + testDefinition.input), options);
			
			if (testDefinition.expectErrorCode != null) {
				Assert.fail("expected '" + testDefinition.expectErrorCode + "' error code");
			}
			
			Assert.assertNotNull(result);
			
		} catch (JsonLdError e) {	
//			e.printStackTrace();
			Assert.assertEquals(testDefinition.expectErrorCode, e.getCode().name());
			return;
		}
		
		RemoteDocument expectedDocument = loader.loadDocument(URI.create(baseUri + testDefinition.expect), new LoadDocumentOptions());
		
//		Map<String, Object> properties = new HashMap<>(1);
//		properties.put(JsonGenerator.PRETTY_PRINTING, true);
//
//		JsonWriterFactory writerFactory = Json.createWriterFactory(properties);

//			JsonWriter jsonWriter2 = writerFactory.createWriter(System.out);
//			jsonWriter2.write(result);
//			jsonWriter2.close();
			
		Assert.assertNotNull(expectedDocument);
		Assert.assertNotNull(expectedDocument.getDocument());
		
		// compare expected with the result		
		Assert.assertEquals(expectedDocument.getDocument().asJsonStructure(), result);
	}

	@Parameterized.Parameters(name = "{1}: {2}")
	public static Collection<Object[]> data() throws IOException {
		
		try (InputStream is = JsonLdExpandTest.class.getResourceAsStream(RESOURCES_BASE + "expand-manifest.jsonld")) {

			Assert.assertNotNull(is);
	
			try (final JsonParser parser = Json.createParser(is)) {
	
				if (!parser.hasNext()) {
					return Collections.emptyList();
				}
				
				parser.next();
				
				final JsonObject manifest = parser.getObject();
				
				String baseUri = manifest.getString("baseIri");
				
				return manifest
						.getJsonArray("sequence")
							.stream()
								.map(JsonValue::asJsonObject)
								.map(o -> JsonLdTestCase.of(o, baseUri))
								.map(o -> new Object[] {o, o.id, o.name, baseUri})
								.collect(Collectors.toList());
			}
		}		
    }
}
