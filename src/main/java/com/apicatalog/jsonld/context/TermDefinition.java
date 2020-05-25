package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

import javax.json.JsonValue;

import com.apicatalog.jsonld.grammar.DirectionType;

public class TermDefinition {

    // mandatory
    String uriMapping;

    boolean prefixFlag;

    boolean protectedFlag;

    boolean reversePropertyFlag;

    // optional
    URI baseUrl;

    JsonValue localContext;

    Collection<String> containerMapping;

    DirectionType directionMapping;

    String indexMapping;

    JsonValue languageMapping;

    String nestValue;

    String typeMapping;

    public TermDefinition(boolean prefixFlag, boolean protectedFlag, boolean reversePropertyFlag) {
        this.prefixFlag = prefixFlag;
        this.protectedFlag = protectedFlag;
        this.reversePropertyFlag = reversePropertyFlag;
        this.containerMapping = new ArrayList<>();
    }

    public void setLocalContext(JsonValue context) {
        this.localContext = context;
    }

    public void setBaseUrl(URI baseUrl) {
        this.baseUrl = baseUrl;
    }

    public String getUriMapping() {
        return uriMapping;
    }

    public void setUriMapping(String uriMapping) {
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

    public void setLanguageMapping(JsonValue languageMapping) {
        this.languageMapping = languageMapping;
    }

    public void addContainerMapping(String mapping) {
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

    public void setIndexMapping(String indexMapping) {
        this.indexMapping = indexMapping;
    }

    public boolean isNotSameExcept(TermDefinition ref) {

        return !Objects.equals(uriMapping, ref.uriMapping) || prefixFlag != ref.prefixFlag
                || !Objects.equals(reversePropertyFlag, ref.reversePropertyFlag)
                || !Objects.equals(baseUrl, ref.baseUrl) || !Objects.equals(containerMapping, ref.containerMapping)
                || !Objects.equals(directionMapping, ref.directionMapping)
                || !Objects.equals(indexMapping, ref.indexMapping) || !Objects.equals(nestValue, ref.nestValue)
                || !Objects.equals(typeMapping, ref.typeMapping);

//TODO 
//        JsonValue localContext;
//        Collection<String> containerMapping;    // any order?
//        JsonValue languageMapping;

    }
    
    public boolean hasContainerMapping(String value) {
        return containerMapping != null && containerMapping.contains(value);
    }
    
}
