package com.apicatalog.jsonld;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.Assert;

import com.apicatalog.gson.JsonParserWrapper;
import com.apicatalog.jsonld.impl.DefaultJsonLdProcessor;

public class JsonLdCompactTestExecutor {

	String resourceDirectory;
	String testPrefix;
	
	public JsonLdCompactTestExecutor(String resourceDirectory, String testPrefix) {
		this.resourceDirectory = resourceDirectory;
		this.testPrefix = testPrefix; 
	}
	
	public void execute() throws IOException, JsonLdError {
		
		Path inPath = Paths.get("src","test","resources", "json-ld-test-suite", resourceDirectory, testPrefix + "-in.jsonld");
		
		Assert.assertTrue(Files.isRegularFile(inPath));
		Assert.assertTrue(Files.isReadable(inPath));
		
		Path contextPath = Paths.get("src","test","resources", "json-ld-test-suite", resourceDirectory, testPrefix + "-context.jsonld");
		Path outPath = Paths.get("src","test","resources", "json-ld-test-suite", resourceDirectory, testPrefix + "-out.jsonld");

		System.out.println("IN " + Files.readString(inPath));
		System.out.println("CTX " + Files.readString(contextPath));
		
		
		
		JsonLdProcessor processor = new DefaultJsonLdProcessor(new JsonParserWrapper());
		
		JsonLdRecord result = processor.compact(JsonLdInput.of(inPath.toUri()), JsonLdContext.of(contextPath.toUri().toString()));

		System.out.println(">>> " + Files.readString(outPath));
		
		Assert.assertNotNull(result);
		
	}
	
}
