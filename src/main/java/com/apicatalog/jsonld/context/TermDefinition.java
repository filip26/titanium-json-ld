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
package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Represents a JSON-LD term definition mapping a term to an IRI and associated
 * processing metadata.
 */
public final class TermDefinition {

    // required
    private String uriMapping;

    private boolean prefixFlag;

    private boolean protectedFlag;

    private boolean reversePropertyFlag;

    // optional
    private URI baseUrl;

    private Object localContext;
    private boolean resetLocalContext;

    private Set<String> containerMapping;

    private String indexMapping;

    private String nestValue;

    private String typeMapping;

    private Direction directionMapping;

    private String languageMapping;

    /**
     * Creates a new {@link TermDefinition} with the specified flags.
     *
     * @param prefixFlag          {@code true} if the term can act as a compact IRI
     *                            prefix
     * @param protectedFlag       {@code true} if the term definition is protected
     *                            against redefinition
     * @param reversePropertyFlag {@code true} if the term defines a reverse
     *                            property
     */
    public TermDefinition(boolean prefixFlag, boolean protectedFlag, boolean reversePropertyFlag) {
        this.prefixFlag = prefixFlag;
        this.protectedFlag = protectedFlag;
        this.reversePropertyFlag = reversePropertyFlag;
        this.containerMapping = new HashSet<>();

        this.localContext = null;
        this.resetLocalContext = false;
    }

    /**
     * Sets the scoped local context associated with this term definition.
     *
     * @param context the scoped context value, or {@code null} to reset it
     */
    public void localContext(Object context) {
        this.localContext = context;
        this.resetLocalContext = context == null;
    }

    /**
     * Checks whether a local context is set or explicitly reset on this term
     * definition.
     *
     * @return {@code true} if a local context is present or reset, {@code false}
     *         otherwise
     */
    public boolean hasLocalContext() {
        return resetLocalContext || localContext != null;
    }

    /**
     * Returns the scoped local context associated with this term definition.
     *
     * @return the local context object, or {@code null} if none is set
     */
    public Object localContext() {
        return localContext;
    }

    /**
     * Returns the IRI mapping for this term.
     *
     * @return the mapped URI string, or {@code null} if unmapped
     */
    public String uriMapping() {
        return uriMapping;
    }

    /**
     * Sets the IRI mapping for this term.
     *
     * @param uriMapping the mapped URI string
     */
    void uriMapping(String uriMapping) {
        this.uriMapping = uriMapping;
    }

    /**
     * Returns the language tag mapping for this term.
     *
     * @return the language mapping string, or {@code null} if none is set
     */
    public String languageMapping() {
        return languageMapping;
    }

    /**
     * Returns the base direction mapping for this term.
     *
     * @return the {@link Direction} mapping, or {@code null} if none is set
     */
    public Direction directionMapping() {
        return directionMapping;
    }

    /**
     * Returns the type mapping for this term.
     *
     * @return the type mapping string, or {@code null} if none is set
     */
    public String typeMapping() {
        return typeMapping;
    }

    /**
     * Checks whether this term can act as a prefix in compact IRIs.
     *
     * @return {@code true} if prefix mode is enabled, {@code false} otherwise
     */
    public boolean isPrefix() {
        return prefixFlag;
    }

    /**
     * Checks whether this term cannot act as a prefix in compact IRIs.
     *
     * @return {@code true} if prefix mode is disabled, {@code false} otherwise
     */
    public boolean isNotPrefix() {
        return !prefixFlag;
    }

    /**
     * Sets the language tag mapping for this term.
     *
     * @param languageMapping the language mapping string
     */
    void languageMapping(String languageMapping) {
        this.languageMapping = languageMapping;
    }

    /**
     * Adds a container keyword to this term definition's container mapping.
     *
     * @param mapping the container keyword to add
     */
    void addContainerMapping(String mapping) {
        this.containerMapping.add(mapping);
    }

    /**
     * Returns the collection of container keywords configured for this term.
     *
     * @return the container mappings collection
     */
    public Collection<String> containerMapping() {
        return containerMapping;
    }

    /**
     * Returns the base URL associated with this term definition.
     *
     * @return the base {@link URI}, or {@code null} if none is set
     */
    public URI baseUrl() {
        return baseUrl;
    }

    /**
     * Sets the base URL associated with this term definition.
     *
     * @param baseUrl the base {@link URI}
     */
    void baseUrl(URI baseUrl) {
        this.baseUrl = baseUrl;
    }

    /**
     * Checks whether this term represents a reverse property.
     *
     * @return {@code true} if this is a reverse property, {@code false} otherwise
     */
    public boolean isReverseProperty() {
        return reversePropertyFlag;
    }

    /**
     * Returns the index mapping property for this term.
     *
     * @return the index mapping string, or {@code null} if none is set
     */
    public String indexMapping() {
        return indexMapping;
    }

    /**
     * Returns the nest property value for this term.
     *
     * @return the nest value string, or {@code null} if none is set
     */
    public String nestValue() {
        return nestValue;
    }

    /**
     * Checks whether this term definition is protected.
     *
     * @return {@code true} if protected, {@code false} otherwise
     */
    public boolean isProtected() {
        return protectedFlag;
    }

    /**
     * Sets the index mapping property for this term.
     *
     * @param indexMapping the index mapping string
     */
    void indexMapping(String indexMapping) {
        this.indexMapping = indexMapping;
    }

    /**
     * Sets whether this term can act as a prefix in compact IRIs.
     *
     * @param prefixFlag {@code true} if prefix mode is enabled
     */
    void prefix(boolean prefixFlag) {
        this.prefixFlag = prefixFlag;
    }

    /**
     * Sets whether this term definition is protected.
     *
     * @param protectedFlag {@code true} if protected
     */
    void protectedFlag(boolean protectedFlag) {
        this.protectedFlag = protectedFlag;
    }

    /**
     * Sets whether this term represents a reverse property.
     *
     * @param reversePropertyFlag {@code true} if this is a reverse property
     */
    void reverseProperty(boolean reversePropertyFlag) {
        this.reversePropertyFlag = reversePropertyFlag;
    }

    /**
     * Sets the type mapping for this term.
     *
     * @param typeMapping the type mapping string
     */
    void typeMapping(String typeMapping) {
        this.typeMapping = typeMapping;
    }

    /**
     * Sets the base direction mapping for this term.
     *
     * @param directionMapping the {@link Direction} mapping
     */
    void directionMapping(Direction directionMapping) {
        this.directionMapping = directionMapping;
    }

    /**
     * Sets the nest property value for this term.
     *
     * @param nestValue the nest value string
     */
    void nestValue(String nestValue) {
        this.nestValue = nestValue;
    }

    /**
     * Checks whether this term definition differs from the given reference
     * definition in fields other than the protected flag.
     *
     * @param ref the reference {@link TermDefinition} to compare against
     * @return {@code true} if any compared attribute differs, {@code false}
     *         otherwise
     */
    public boolean isNotSameExcept(TermDefinition ref) {
        return !Objects.equals(uriMapping, ref.uriMapping)
                || prefixFlag != ref.prefixFlag
                || !Objects.equals(reversePropertyFlag, ref.reversePropertyFlag)
                || !Objects.equals(baseUrl, ref.baseUrl)
                || !Objects.equals(containerMapping, ref.containerMapping)
                || !Objects.equals(directionMapping, ref.directionMapping)
                || !Objects.equals(indexMapping, ref.indexMapping)
                || !Objects.equals(nestValue, ref.nestValue)
                || !Objects.equals(typeMapping, ref.typeMapping)
                || !Objects.equals(languageMapping, ref.languageMapping)
                || !Objects.deepEquals(localContext, ref.localContext);
    }

    /**
     * Checks whether this term definition includes the specified container mapping.
     *
     * @param value the container keyword to check
     * @return {@code true} if present in container mappings, {@code false}
     *         otherwise
     */
    public boolean hasContainerMapping(String value) {
        return containerMapping != null && containerMapping.contains(value);
    }

    @Override
    public String toString() {
        return "TermDefinition[uri=" + uriMapping + ", "
                + "baseUri=" + baseUrl + ", "
                + "indexMapping=" + indexMapping + ", "
                + "typeMapping=" + typeMapping + ", "
                + "containerMapping=" + containerMapping
                + "]";
    }
}