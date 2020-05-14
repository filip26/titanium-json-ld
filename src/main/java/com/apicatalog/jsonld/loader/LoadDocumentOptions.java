package com.apicatalog.jsonld.loader;

import java.util.Collection;

/**
 * The {@link LoadDocumentOptions} type is used to pass various options to the {@link LoadDocumentCallback}.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#loaddocumentoptions">LoadDocumentOptions Specification</a>
 *
 */
public class LoadDocumentOptions {

	private boolean extractAllScripts;
	
	private String profile;
	
	private Collection<String> requestProfile;
	
	public LoadDocumentOptions() {
		this.extractAllScripts = false;
		this.profile = null;
		this.requestProfile = null;
	}
	
	public boolean isExtractAllScripts() {
		return extractAllScripts;
	}
	
	public LoadDocumentOptions setExtractAllScripts(boolean extractAllScripts) {
		this.extractAllScripts = extractAllScripts;
		return this;
	}
	
	public String getProfile() {
		return profile;
	}
	
	public void setProfile(String profile) {
		this.profile = profile;
	}
	
	public Collection<String> getRequestProfile() {
		return requestProfile;
	}
	
	public void setRequestProfile(Collection<String> requestProfile) {
		this.requestProfile = requestProfile;
	}
	
}
