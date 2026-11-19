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
package com.apicatalog.jsonld.framing;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.lang.Embed;

public final class FramingState {

    private Embed embed;
    private boolean requireAll;
    private boolean embedded;
    private boolean explicitInclusion;
    private boolean omitDefault;

    private String graphName;

    private NodeMap graphMap;

    private Deque<String> parents;
    private Set<String> parentIndex;

    private Map<String, Set<String>> uniqueEmbeds;

    private Map<String, Map<String, Map<String, Set<String>>>> reversePropertyIndex;

    public FramingState() {
        this.parents = new ArrayDeque<>();
        this.parentIndex = new LinkedHashSet<>();
        this.uniqueEmbeds = new HashMap<>();
    }

    public FramingState(FramingState state) {
        this.embed = state.embed;
        this.requireAll = state.requireAll;
        this.embedded = state.embedded;
        this.explicitInclusion = state.explicitInclusion;
        this.omitDefault = state.omitDefault;
        this.graphMap = state.graphMap;
        this.graphName = state.graphName;
        this.parents =  state.parents;
        this.parentIndex = state.parentIndex;
        this.uniqueEmbeds = state.uniqueEmbeds;
        this.reversePropertyIndex = state.reversePropertyIndex;
    }

    public Embed getEmbed() {
        return embed;
    }

    public void setEmbed(Embed embed) {
        this.embed = embed;
    }

    public boolean isRequireAll() {
        return requireAll;
    }

    public void setRequireAll(boolean requireAll) {
        this.requireAll = requireAll;
    }

    public boolean isEmbedded() {
        return embedded;
    }

    public void setEmbedded(boolean embedded) {
        this.embedded = embedded;
    }

    public boolean isExplicitInclusion() {
        return explicitInclusion;
    }

    public void setExplicitInclusion(boolean explicitInclusion) {
        this.explicitInclusion = explicitInclusion;
    }

    public boolean isOmitDefault() {
        return omitDefault;
    }

    public void setOmitDefault(boolean omitDefault) {
        this.omitDefault = omitDefault;
    }

    public String getGraphName() {
        return graphName;
    }

    public void setGraphName(String graphName) {
        this.graphName = graphName;
    }

    public NodeMap getGraphMap() {
        return graphMap;
    }

    public void setGraphMap(NodeMap graphMap) {
        this.graphMap = graphMap;
    }

    public boolean isParent(String subject) {
        return parentIndex.contains(parentKey(graphName, subject));
    }

    public void addParent(String subject) {
        final var parentKey = parentKey(graphName, subject);
        parents.push(parentKey);
        parentIndex.add(parentKey);
    }

    public void removeLastParent() {
        parentIndex.remove(parents.pop());
    }

    public Set<String> getReversePropertySubjects(String graphName, String property, String value) {
        if (reversePropertyIndex == null) {
            return Set.of();
        }

        final var graphIndex = reversePropertyIndex.get(graphName);
        if (graphIndex == null) {
            return Set.of();
        }

        final var propertyIndex = graphIndex.get(property);
        if (propertyIndex == null) {
            return Set.of();
        }

        final var subjects = propertyIndex.get(value);
        return subjects != null ? subjects : Set.of();
    }

    public void setReversePropertyIndex(Map<String, Map<String, Map<String, Set<String>>>> index) {
        this.reversePropertyIndex = index;
    }

    public Map<String, ?> getSubject(String subject) {
        return graphMap.find(graphName, subject).orElse(Map.of());
    }

    public void resetUniqueEmbeds() {
        uniqueEmbeds.clear();
        uniqueEmbeds.computeIfAbsent(graphName, x -> new LinkedHashSet<>());
    }

    public boolean isEmbedded(String subject) {
        return uniqueEmbeds
                .getOrDefault(graphName, Set.of())
                .contains(subject);
    }

    public void markEmbedded(String subject) {
        uniqueEmbeds
                .computeIfAbsent(graphName, x -> new LinkedHashSet<>())
                .add(subject);
    }

    private static String parentKey(String graphName, String subject) {
        return graphName + "@@@" + subject;
    }
}
