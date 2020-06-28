package com.apicatalog.jsonld.loader;

import java.util.Collection;

/**
 * The {@link DocumentLoaderOptions} type is used to pass various options to the
 * {@link DocumentLoader}.
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#loaddocumentoptions">LoadDocumentOptions
 *      Specification</a>
 *
 */
public class DocumentLoaderOptions {

    private boolean extractAllScripts;

    private String profile;

    private Collection<String> requestProfile;

    public DocumentLoaderOptions() {
        this.extractAllScripts = false;
        this.profile = null;
        this.requestProfile = null;
    }

    public boolean isExtractAllScripts() {
        return extractAllScripts;
    }

    public DocumentLoaderOptions setExtractAllScripts(boolean extractAllScripts) {
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
