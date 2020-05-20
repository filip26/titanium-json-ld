package com.apicatalog.jsonld;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonParser;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.impl.DefaultJsonLdProcessor;

@RunWith(Parameterized.class)
public class JsonLdProcessorExpandTest {

	@Parameterized.Parameter(0)
	public JsonLdTestDefinition testDefinition;

	@Parameterized.Parameter(1)
	public String testId;
	
	@Parameterized.Parameter(2)
	public String testName;
		
	@Parameterized.Parameter(3)
	public String baseUri;
	
	@Test
	public void testExpand() throws IOException, JsonLdError {

		Map<String, Object> properties = new HashMap<>(1);
		properties.put(JsonGenerator.PRETTY_PRINTING, true);

		JsonWriterFactory writerFactory = Json.createWriterFactory(properties);

		final Path inputPath = Paths.get("src","test","resources", "json-ld-test-suite", testDefinition.input);
		
		final JsonLdProcessor processor = new DefaultJsonLdProcessor();
		
		JsonValue result = null;
		
		JsonLdOptionsBuilder optionsBuilder = new JsonLdOptionsBuilder().baseUri(URI.create(baseUri + testDefinition.input));
		
		if (testDefinition.options != null) {
			testDefinition.options.setup(optionsBuilder);
		}
		
		optionsBuilder.ordered(true);	//FIXME ?!
		
		JsonLdOptions options = optionsBuilder.create();
		
		try {
			result = processor.expand(inputPath.toUri(), options);
			
			if (testDefinition.expectErrorCode != null) {
				Assert.fail("expected '" + testDefinition.expectErrorCode + "' error code");
			}
			
			Assert.assertNotNull(result);
			
		} catch (JsonLdError e) {	
//			e.printStackTrace();
			Assert.assertEquals(testDefinition.expectErrorCode, e.getCode().name());
			return;
		}

		final Path expectPath = Paths.get("src","test","resources", "json-ld-test-suite", testDefinition.expect);
		
		// compare expected with the result
		try (final JsonParser parser = Json.createParser(Files.newBufferedReader(expectPath))) {

			parser.next();
			
			final JsonValue expected = parser.getValue();
			
//			JsonWriter jsonWriter2 = writerFactory.createWriter(System.out);
//			jsonWriter2.write(result);
//			jsonWriter2.close();
			
			Assert.assertEquals(expected, result);

		}
	}

	@Parameterized.Parameters(name = "{1}: {2}")
	public static Collection<Object[]> data() throws IOException {
		
		final Path manifestPath = Paths.get("src","test","resources", "json-ld-test-suite", "expand-manifest.jsonld");
		
		Assert.assertTrue(Files.isRegularFile(manifestPath));
		Assert.assertTrue(Files.isReadable(manifestPath));

		try (final JsonParser parser = Json.createParser(Files.newBufferedReader(manifestPath))) {

			if (!parser.hasNext()) {
				return Collections.emptyList();
			}
			
			parser.next();
			
			final JsonObject manifest = parser.getObject();
			
			return manifest
					.getJsonArray("sequence")
						.stream()
							.map(JsonValue::asJsonObject)
							.map(JsonLdTestDefinition::of)
							.map(o -> new Object[] {o, o.id, o.name, ((JsonString)(manifest.get("baseIri"))).getString()})
							.collect(Collectors.toList());
		}
    }
}
