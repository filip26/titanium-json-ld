/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package no.hasmac.jsonld.loader;

import java.util.Collection;

/**
 * The {@link DocumentLoaderOptions} is used to pass various options to the
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

    public void setExtractAllScripts(boolean extractAllScripts) {
        this.extractAllScripts = extractAllScripts;
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
