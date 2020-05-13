package com.apicatalog.jsonld.impl;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.json.JsonParser;
import com.apicatalog.jsonld.JsonLdContext;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdInput;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdProcessor;
import com.apicatalog.jsonld.JsonLdRecord;
import com.apicatalog.jsonld.alg.Expansion;
import com.apicatalog.rdf.RdfDataset;

public class DefaultJsonLdProcessor implements JsonLdProcessor {

	private final JsonParser jsonParser;
	
	public DefaultJsonLdProcessor(JsonParser jsonParser) {
		this.jsonParser = jsonParser;
	}
	
	@Override
	public JsonLdRecord compact(JsonLdInput input) throws JsonLdError {
		return compact(input, JsonLdOptions.DEFAULT);
	}

	@Override
	public JsonLdRecord compact(JsonLdInput input, JsonLdContext context) throws JsonLdError {
		return compact(input, context, JsonLdOptions.DEFAULT);
	}

	@Override
	public JsonLdRecord compact(JsonLdInput input, JsonLdOptions options) throws JsonLdError {
		return compact(input, null, options);
	}

	@Override
	public JsonLdRecord compact(JsonLdInput input, JsonLdContext context, JsonLdOptions options) throws JsonLdError {

		Collection<JsonLdRecord> records = input.getRecords(options, jsonParser);
		
		System.out.println("-> " + records);
		
		
		return null;
	}

	@Override
	public Collection<JsonLdRecord> expand(JsonLdInput input) throws JsonLdError {
		return expand(input, JsonLdOptions.DEFAULT);
	}

	@Override
	public Collection<JsonLdRecord> expand(JsonLdInput input, JsonLdOptions options) throws JsonLdError {

		final Collection<JsonLdRecord> records = input.getRecords(options, jsonParser);
		
		if (records.isEmpty()) {
			return Collections.emptyList();
		}
		
		JsonLdContext context = new JsonLdContextImpl();

		System.out.println("-> " + records);
				
		return Expansion.expand(context, records, null, null);
	}

	@Override
	public JsonLdRecord flatten(JsonLdInput input) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public JsonLdRecord flatten(JsonLdInput input, JsonLdContext context) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public JsonLdRecord flatten(JsonLdInput input, JsonLdOptions options) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public JsonLdRecord flatten(JsonLdInput input, JsonLdContext context, JsonLdOptions options) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<JsonLdRecord> fromRdf(RdfDataset input) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<JsonLdRecord> fromRdf(RdfDataset input, JsonLdOptions options) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public RdfDataset toRdf(JsonLdInput input) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public RdfDataset toRdf(JsonLdInput input, JsonLdOptions options) {
		// TODO Auto-generated method stub
		return null;
	}

}
