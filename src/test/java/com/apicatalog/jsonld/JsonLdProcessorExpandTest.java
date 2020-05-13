package com.apicatalog.jsonld;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.gson.JsonParserWrapper;
import com.apicatalog.jsonld.impl.DefaultJsonLdProcessor;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonIOException;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

@RunWith(Parameterized.class)
public class JsonLdProcessorExpandTest {

	@Parameterized.Parameter(0)
	public JsonLdTestDefinition testDefinition;

	@Parameterized.Parameter(1)
	public String testId;
	
	@Parameterized.Parameter(2)
	public String testName;
		
	@Test
	public void testExpand() throws IOException, JsonLdError {
		
		final Path inputPath = Paths.get("src","test","resources", "json-ld-test-suite", testDefinition.input);
		
		final JsonLdProcessor processor = new DefaultJsonLdProcessor(new JsonParserWrapper());
		
		try {
			Collection<JsonLdRecord> result = processor.expand(JsonLdInput.of(inputPath.toUri()));
			
			if (testDefinition.expectErrorCode != null) {
				Assert.fail("Expected '" + testDefinition.expectErrorCode + "' error code");
			}
			
			Assert.assertNotNull(result);
			
			final Path expectPath = Paths.get("src","test","resources", "json-ld-test-suite", testDefinition.expect);
			
			final JsonElement expected = JsonParser.parseReader(Files.newBufferedReader(expectPath));
			
			Assert.assertEquals(expected, result);
			
		} catch (JsonLdError e) {			
			Assert.assertEquals(testDefinition.expectErrorCode, e.getCode().name());
		}
	}

	@Parameterized.Parameters(name = "{1}: {2}")
	public static Collection<Object[]> data() throws JsonIOException, JsonSyntaxException, IOException {
		
		final Path manifestPath = Paths.get("src","test","resources", "json-ld-test-suite", "expand-manifest.jsonld");
		
		Assert.assertTrue(Files.isRegularFile(manifestPath));
		Assert.assertTrue(Files.isReadable(manifestPath));

		final Map<String, Object> manifest = (new Gson()).fromJson(Files.newBufferedReader(manifestPath), (new TypeToken<Map<String, Object>>() {}).getType());
		
		@SuppressWarnings("unchecked")
		final ArrayList<Map<String, Object>> sequence = (ArrayList<Map<String,Object>>) manifest.get("sequence");
		
		final ArrayList<Object[]> data = new ArrayList<>(sequence.size());

		for (final Map<String, Object> test : sequence) {
			
			final JsonLdTestDefinition testDefinition = new JsonLdTestDefinition();
			testDefinition.id = (String) test.get("@id");
			testDefinition.input = (String) test.get("input");
			testDefinition.expect = (String) test.get("expect");
			
			testDefinition.expectErrorCode((String) test.get("expectErrorCode"));

			data.add(new Object[] {testDefinition, test.get("@id"), test.get("name")});
		}

		return data;
    }
}
