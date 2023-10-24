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
package no.hasmac.jsonld.context;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import no.hasmac.jsonld.lang.DirectionType;

import jakarta.json.JsonValue;

public final class TermDefinition {

    // required
    private String uriMapping;

    private boolean prefixFlag;

    private boolean protectedFlag;

    private boolean reversePropertyFlag;

    // optional
    private URI baseUrl;

    private JsonValue localContext;

    private Set<String> containerMapping;

    private String indexMapping;

    private String nestValue;

    private String typeMapping;

    private DirectionType directionMapping;

    private JsonValue languageMapping;

    public TermDefinition(boolean prefixFlag, boolean protectedFlag, boolean reversePropertyFlag) {
        this.prefixFlag = prefixFlag;
        this.protectedFlag = protectedFlag;
        this.reversePropertyFlag = reversePropertyFlag;
        this.containerMapping = new HashSet<>(1);
    }

    public void setLocalContext(JsonValue context) {
        this.localContext = context;
    }

    protected void setBaseUrl(URI baseUrl) {
        this.baseUrl = baseUrl;
    }

    public String getUriMapping() {
        return uriMapping;
    }

    protected void setUriMapping(String uriMapping) {
        this.uriMapping = uriMapping;
    }

    public JsonValue getLanguageMapping() {
        return languageMapping;
    }

    public DirectionType getDirectionMapping() {
        return directionMapping;
    }

    public String getTypeMapping() {
        return typeMapping;
    }

    public boolean isPrefix() {
        return prefixFlag;
    }

    public boolean isNotPrefix() {
        return !prefixFlag;
    }

    protected void setLanguageMapping(JsonValue languageMapping) {
        this.languageMapping = languageMapping;
    }

    protected void addContainerMapping(String mapping) {
        this.containerMapping.add(mapping);
    }

    public Collection<String> getContainerMapping() {
        return containerMapping;
    }

    public JsonValue getLocalContext() {
        return localContext;
    }

    public URI getBaseUrl() {
        return baseUrl;
    }

    public boolean hasLocalContext() {
        return localContext != null;
    }

    public boolean isReverseProperty() {
        return reversePropertyFlag;
    }

    public String getIndexMapping() {
        return indexMapping;
    }

    public String getNestValue() {
        return nestValue;
    }

    public boolean isProtected() {
        return protectedFlag;
    }

    protected void setIndexMapping(String indexMapping) {
        this.indexMapping = indexMapping;
    }

    protected void setContainerMapping(Set<String> containerMapping) {
        this.containerMapping = containerMapping;
    }

    protected void setPrefix(boolean prefixFlag) {
        this.prefixFlag = prefixFlag;
    }

    protected void setProtected(boolean protectedFlag) {
        this.protectedFlag = protectedFlag;
    }

    protected void setReverseProperty(boolean reversePropertyFlag) {
        this.reversePropertyFlag = reversePropertyFlag;
    }

    protected void setTypeMapping(String typeMapping) {
        this.typeMapping = typeMapping;
    }

    protected void setDirectionMapping(DirectionType directionMapping) {
        this.directionMapping = directionMapping;
    }

    protected void setNestValue(String nestValue) {
        this.nestValue = nestValue;
    }

    public boolean isNotSameExcept(TermDefinition ref) {
        return !Objects.equals(uriMapping, ref.uriMapping) || prefixFlag != ref.prefixFlag
                || !Objects.equals(reversePropertyFlag, ref.reversePropertyFlag)
                || !Objects.equals(baseUrl, ref.baseUrl) || !Objects.equals(containerMapping, ref.containerMapping)
                || !Objects.equals(directionMapping, ref.directionMapping)
                || !Objects.equals(indexMapping, ref.indexMapping) || !Objects.equals(nestValue, ref.nestValue)
                || !Objects.equals(typeMapping, ref.typeMapping)
                || !Objects.equals(languageMapping, ref.languageMapping)
                || !Objects.equals(localContext, ref.localContext)
                ;
    }

    public boolean hasContainerMapping(String value) {
        return containerMapping != null && containerMapping.contains(value);
    }

    @Override
    public String toString() {
        return "TermDefinition[uriMapping=" + uriMapping + ", "
                    + "localContext=" + localContext + ", "
                    + "indexMapping=" + indexMapping + ", "
                    + "typeMapping=" + typeMapping + "]";
    }
}
