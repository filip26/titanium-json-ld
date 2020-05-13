package com.apicatalog.jsonld;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.junit.Test;

public class JsonLdCompactTest {

	
	@Test
	public void testCompaction() throws IOException, JsonLdError {
		
		(new JsonLdCompactTestExecutor("compact", "0001")).execute();		
		

	}
	
}
